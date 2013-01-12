import os
import sublime
import sublime_plugin
from threading import Thread

from sublime_haskell_common import get_cabal_project_dir_and_name_of_view, call_and_wait, get_setting, get_setting_async, set_setting, save_settings, get_haskell_command_window_view_file_project, attach_sandbox
from parseoutput import run_chain_build_thread
from autocomplete import autocompletion

OUTPUT_PANEL_NAME = "haskell_run_output"

cabal_tool = {
    True: {'command': 'cabal-dev', 'message': 'Cabal-Dev', 'extra': lambda cmd: attach_sandbox(cmd)},
    False: {'command': 'cabal', 'message': 'Cabal', 'extra': lambda cmd: cmd},
}

cabal_command = {
    'clean': {'steps': ['clean'], 'message': 'Cleaning'},
    'configure': {'steps': ['configure'], 'message': 'Configure'},
    'build': {'steps': ['build'], 'message': 'Building'},
    'rebuild': {'steps': ['clean', 'configure', 'build'], 'message': 'Rebuilding'},
    'install': {'steps': ['install'], 'message': 'Installing'}
}


# Base command
class SublimeHaskellBaseCommand(sublime_plugin.WindowCommand):
    def is_enabled(self):
        return len(autocompletion.projects) > 0

    def build(self, command, use_cabal_dev=None):
        select_project(
            self.window,
            lambda n, d: run_build(self.window.active_view(), n, d, command, use_cabal_dev))


# Select project from list
# on_selected accepts name of project and directory of project
def select_project(window, on_selected):
    ps = autocompletion.projects.items()

    def run_selected(psel):
        on_selected(psel[0], psel[1]['dir'])

    if len(ps) == 0:
        return
    if len(ps) == 1:  # There's only one project, build it
        run_selected(ps[0])
        return

    cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(window.active_view())

    # Show current project first
    def compare(l, r):
        res = cmp(not (l[0] == cabal_project_name), not (r[0] == cabal_project_name))
        if res == 0:
            res = cmp(l[0], r[0])
        return res

    ps.sort(compare)

    def on_done(idx):
        if idx != -1:
            run_selected(ps[idx])

    window.show_quick_panel(map(lambda m: [m[0], m[1]['dir']], ps), on_done)


def run_build(view, project_name, project_dir, command, use_cabal_dev=None):
    # Run cabal or cabal-dev
    if use_cabal_dev == None:
        use_cabal_dev = get_setting_async('use_cabal_dev')

    tool = cabal_tool[use_cabal_dev]
    config = cabal_command[command]

    # Title of tool: Cabal, Cabal-Dev
    tool_title = tool['message']
    # Title of action: Cleaning, Building, etc.
    action_title = config['message']
    # Extra arguments lambda
    extra_args = tool['extra']
    # Tool name: cabal, cabal-dev
    tool_name = tool['command']
    # Tool arguments (commands): build, clean, etc.
    tool_steps = config['steps']

    run_chain_build_thread(
        view,
        project_dir,
        tool_title + ': ' + action_title + ' ' + project_name,
        [extra_args([tool_name, step]) for step in tool_steps])


class SublimeHaskellSwitchCabalDev(sublime_plugin.WindowCommand):
    def run(self):
        use_cabal_dev = get_setting('use_cabal_dev')
        sandbox = get_setting('cabal_dev_sandbox')
        sandboxes = get_setting('cabal_dev_sandbox_list')

        sandboxes.append(sandbox)
        sandboxes = list(set(sandboxes))
        set_setting('cabal_dev_sandbox_list', sandboxes)

        # No candboxes
        if len(sandboxes) == 0:
            sublime.status_message('SublimeHaskell: There is nothing to switch to')
            set_setting('use_cabal_dev', False)
            save_settings()
            return

        # One sandbox, just switch
        if len(sandboxes) == 1:
            set_setting('use_cabal_dev', not use_cabal_dev)
            if use_cabal_dev:
                now_using = 'Cabal'
            else:
                now_using = 'Cabal-Dev'
            sublime.status_message('SublimeHaskell: Switched to ' + now_using)
            save_settings()
            return

        # Many sandboxes, show list
        self.sorted_sands = sandboxes
        # Move previously used sandbox (or cabal) on top
        self.sorted_sands.remove(sandbox)
        if use_cabal_dev:
            self.sorted_sands.insert(0, sandbox)
            self.sorted_sands.insert(0, "<Cabal>")
        else:
            self.sorted_sands.insert(0, "<Cabal>")
            self.sorted_sands.insert(0, sandbox)

        self.window.show_quick_panel(self.sorted_sands, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        selected = self.sorted_sands[idx]
        if selected == "<Cabal>":
            set_setting('use_cabal_dev', False)
        else:
            set_setting('use_cabal_dev', True)
            set_setting('cabal_dev_sandbox', selected)

        save_settings()


# Default build system (cabal or cabal-dev)

class SublimeHaskellClean(SublimeHaskellBaseCommand):
    def run(self):
        self.build('clean')


class SublimeHaskellConfigure(SublimeHaskellBaseCommand):
    def run(self):
        self.build('configure')


class SublimeHaskellBuild(SublimeHaskellBaseCommand):
    def run(self):
        self.build('build')


class SublimeHaskellRebuild(SublimeHaskellBaseCommand):
    def run(self):
        self.build('rebuild')


class SublimeHaskellInstall(SublimeHaskellBaseCommand):
    def run(self):
        self.build('install')


# Auto build current project
class SublimeHaskellBuildAuto(SublimeHaskellBaseCommand):
    def run(self):
        current_project_dir, current_project_name = get_cabal_project_dir_and_name_of_view(self.window.active_view())
        if current_project_name and current_project_dir:
            run_build(self.window.active_view(), current_project_name, current_project_dir, 'build', None)


class SublimeHaskellRun(SublimeHaskellBaseCommand):
    def run(self):
        self.executables = []
        ps = []
        for p, info in autocompletion.projects.items():
            for e in info['executables']:
                ps.append((p + ": " + e['name'], {
                    'dir': info['dir'],
                    'name': e['name']
                }))

        # Nothing to run
        if len(ps) == 0:
            sublime.status_message('SublimeHaskell: Nothing to run')
            return

        cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(self.window.active_view())

        # Show current project first
        def compare(l, r):
            res = cmp(not l[0].startswith(cabal_project_name), not r[0].startswith(cabal_project_name))
            if res == 0:
                res = cmp(l[0], r[0])
            return res

        ps.sort(compare)

        self.executables = map(lambda m: m[1], ps)
        self.window.show_quick_panel(map(lambda m: m[0], ps), self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        selected = self.executables[idx]
        name = selected['name']
        base_dir = selected['dir']
        bin_file = os.path.join(selected['dir'], 'dist', 'build', name, name)

        hide_output(self.window)

        sublime.status_message('SublimeHaskell: Running ' + name + "...")

        # Run in thread
        thread = Thread(
            target=run_binary,
            args=(name, bin_file, base_dir))
        thread.start()


def run_binary(name, bin_file, base_dir):
    exit_code, out, err = call_and_wait(bin_file, cwd=base_dir)
    window = sublime.active_window()
    if not window:
        return
    if exit_code == 0:
        sublime.set_timeout(lambda: sublime.status_message('SublimeHaskell: Running ' + name + u" \u2714"), 0)
        sublime.set_timeout(lambda: write_output(window, out, base_dir), 0)
    else:
        sublime.set_timeout(lambda: sublime.status_message('SublimeHaskell: Running ' + name + u" \u2717"), 0)
        sublime.set_timeout(lambda: write_output(window, err, base_dir), 0)


def write_output(window, text, base_dir):
    "Write text to Sublime's output panel."
    output_view = window.get_output_panel(OUTPUT_PANEL_NAME)
    output_view.set_read_only(False)
    # Configure Sublime's error message parsing:
    output_view.settings().set("result_base_dir", base_dir)
    # Write to the output buffer:
    edit = output_view.begin_edit()
    output_view.insert(edit, 0, text)
    output_view.end_edit(edit)
    # Set the selection to the beginning of the view so that "next result" works:
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0))
    output_view.set_read_only(True)
    # Show the results panel:
    window.run_command('show_panel', {'panel': 'output.' + OUTPUT_PANEL_NAME})


def hide_output(window):
    window.run_command('hide_panel', {'panel': 'output.' + OUTPUT_PANEL_NAME})


# Cabal build system

class SublimeHaskellCabalClean(SublimeHaskellBaseCommand):
    def run(self):
        self.build('clean', False)


class SublimeHaskellCabalConfigure(SublimeHaskellBaseCommand):
    def run(self):
        self.build('configure', False)


class SublimeHaskellCabalBuild(SublimeHaskellBaseCommand):
    def run(self):
        self.build('build', False)


class SublimeHaskellCabalRebuild(SublimeHaskellBaseCommand):
    def run(self):
        self.build('rebuild', False)


class SublimeHaskellCabalInstall(SublimeHaskellBaseCommand):
    def run(self):
        self.build('install', False)


# Cabal-Dev build system

class SublimeHaskellCabalDevClean(SublimeHaskellBaseCommand):
    def run(self):
        self.build('clean', True)


class SublimeHaskellCabalDevConfigure(SublimeHaskellBaseCommand):
    def run(self):
        self.build('configure', True)


class SublimeHaskellCabalDevBuild(SublimeHaskellBaseCommand):
    def run(self):
        self.build('build', True)


class SublimeHaskellCabalDevRebuild(SublimeHaskellBaseCommand):
    def run(self):
        self.build('rebuild', True)


class SublimeHaskellCabalDevInstall(SublimeHaskellBaseCommand):
    def run(self):
        self.build('install', True)


def run_build_commands_with(msg, cmds):
    """Run general build commands"""
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project()
    if not file_shown_in_view:
        return
    syntax_file_for_view = view.settings().get('syntax').lower()
    if 'haskell' not in syntax_file_for_view:
        return
    cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(view)
    if not cabal_project_dir:
        return

    run_chain_build_thread(view, cabal_project_dir, msg(cabal_project_name), cmds)


def run_build_command_with(msg, cmd):
    """Run one command"""
    run_build_commands_with(msg, [cmd])
