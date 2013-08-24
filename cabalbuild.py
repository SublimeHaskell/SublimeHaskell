import os
import sublime
import sublime_plugin
import copy
from threading import Thread

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    from parseoutput import run_chain_build_thread
    from autocomplete import autocompletion
else:
    from SublimeHaskell.sublime_haskell_common import *
    from SublimeHaskell.parseoutput import run_chain_build_thread
    from SublimeHaskell.autocomplete import autocompletion

OUTPUT_PANEL_NAME = "haskell_run_output"

cabal_tool = {
    True: {'command': 'cabal-dev', 'message': 'Cabal-Dev', 'extra': lambda cmd: attach_sandbox(cmd)},
    False: {'command': 'cabal', 'message': 'Cabal', 'extra': lambda cmd: cmd},
}

cabal_config = {
    'clean': {'steps': [['clean']], 'message': 'Cleaning'},
    'configure': {'steps': [['configure', '--enable-tests']], 'message': 'Configure'},
    'build': {'steps': [['build']], 'message': 'Building'},
    'typecheck': {'steps': [['build', '--ghc-options=-c']], 'message': 'Checking'},
    # Commands with warnings:
    # Run fast, incremental build first. Then build everything with -Wall and -fno-code
    # If the incremental build fails, the second step is not executed.
    'build_then_warnings': {'steps': [['build'], ['build', '-v0', '--ghc-options=-fforce-recomp -Wall -fno-code']], 'message': 'Building'},
    'typecheck_then_warnings': {'steps': [['build', '--ghc-options=-c'], ['build', '-v0', '--ghc-options=-fforce-recomp -Wall -fno-code']], 'message': 'Checking'},

    'rebuild': {'steps': [['clean'], ['configure', '--enable-tests'], ['build']], 'message': 'Rebuilding'},
    'install': {'steps': [['install', '--enable-tests']], 'message': 'Installing'},
    'test': {'steps': [['test']], 'message': 'Testing'}
}


# GLOBAL STATE

# Contains names of projects currently being built.
# To be updated only from the UI thread.
projects_being_built = set()


# Base command
class SublimeHaskellBaseCommand(sublime_plugin.WindowCommand):
    def is_enabled(self):
        return len(autocompletion.projects.object) > 0

    def build(self, command, use_cabal_dev=None, filter_project = None):
        select_project(
            self.window,
            lambda n, d: run_build(self.window.active_view(), n, d, cabal_config[command], use_cabal_dev),
            filter_project = filter_project)


# Select project from list
# on_selected accepts name of project and directory of project
# filter_project accepts name of project and project-info as it appears in AutoCompletion object
#   and returns whether this project must appear in selection list
def select_project(window, on_selected, filter_project = None):
    ps = [(name, info) for (name, info) in autocompletion.projects.object.items() if not filter_project or filter_project(name, info)]

    def run_selected(psel):
        on_selected(psel[0], psel[1]['dir'])

    if len(ps) == 0:
        return
    if len(ps) == 1:  # There's only one project, build it
        run_selected(ps[0])
        return

    cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(window.active_view())

    # Returns tuple to sort by
    #   is this project is current? return False to be first on sort
    #   name of project to sort alphabetically
    def compare(proj_name):
        return (proj_name != cabal_project_name, proj_name)

    ps.sort(key = lambda p: compare(p[0]))

    def on_done(idx):
        if idx != -1:
            run_selected(ps[idx])

    window.show_quick_panel(list(map(lambda m: [m[0], m[1]['dir']], ps)), on_done)


def run_build(view, project_name, project_dir, config, use_cabal_dev=None):
    global projects_being_built

    # Don't build if a build is already running for this project
    # We compare the project_name for simplicity (projects with same
    # names are of course possible, but unlikely, so we let them wait)
    if project_name in projects_being_built:
        log("Not building '%s' because it is already being built" % project_name)
        sublime_status_message('Already building %s' % project_name)
        return
    # Set project as building
    projects_being_built.add(project_name)

    # Run cabal or cabal-dev
    if use_cabal_dev is None:
        use_cabal_dev = get_setting_async('use_cabal_dev')

    tool = cabal_tool[use_cabal_dev]

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

    # Assemble command lines to run (possibly multiple steps)
    commands = [extra_args([tool_name] + step) for step in tool_steps]

    log('running build commands: {0}'.format(commands))

    def done_callback():
        # Set project as done being built so that it can be built again
        projects_being_built.remove(project_name)

    # Run them
    run_chain_build_thread(
        view,
        project_dir,
        tool_title + ': ' + action_title + ' ' + project_name,
        commands,
        on_done=done_callback)


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
            sublime_status_message('There is nothing to switch to')
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
            sublime_status_message('Switched to ' + now_using)
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
        self.build('build_then_warnings')


class SublimeHaskellTypecheck(SublimeHaskellBaseCommand):
    def run(self):
        self.build('typecheck_then_warnings')


class SublimeHaskellRebuild(SublimeHaskellBaseCommand):
    def run(self):
        self.build('rebuild')


class SublimeHaskellInstall(SublimeHaskellBaseCommand):
    def run(self):
        self.build('install')

class SublimeHaskellTest(SublimeHaskellBaseCommand):
    def run(self):
        def has_tests(name, info):
            return len(info['tests']) > 0

        self.build('test', filter_project = has_tests)

# Auto build current project
class SublimeHaskellBuildAuto(SublimeHaskellBaseCommand):
    def run(self):
        current_project_dir, current_project_name = get_cabal_project_dir_and_name_of_view(self.window.active_view())
        if current_project_name and current_project_dir:
            build_mode = get_setting('auto_build_mode')
            run_tests = get_setting('auto_run_tests')

            build_command = {
               'normal': 'build',
               'normal-then-warnings': 'build_then_warnings',
               'typecheck': 'typecheck',
               'typecheck-then-warnings': 'typecheck_then_warnings',
            }.get(build_mode)

            if not build_command:
                output_error(self.window, "SublimeHaskell: invalid auto_build_mode '%s'" % build_mode)

            config = copy.deepcopy(cabal_config[build_command])

            if run_tests:
                has_tests = False

                with autocompletion.projects as projects:
                    if current_project_name in projects:
                        has_tests = len(projects[current_project_name]['tests']) > 0

                if has_tests:
                    config['steps'].extend(cabal_config['test']['steps'])

            run_build(self.window.active_view(), current_project_name, current_project_dir, config, None)


class SublimeHaskellRun(SublimeHaskellBaseCommand):
    def run(self):
        self.executables = []
        ps = []
        with autocompletion.projects as projects:
            for p, info in projects.items():
                for e in info['executables']:
                    ps.append((p + ": " + e['name'], {
                        'dir': info['dir'],
                        'name': e['name']
                    }))

        # Nothing to run
        if len(ps) == 0:
            sublime_status_message('Nothing to run')
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

        # Run in thread
        thread = Thread(
            target=run_binary,
            args=(name, bin_file, base_dir))
        thread.start()


def run_binary(name, bin_file, base_dir):
    with status_message_process('Running {0}'.format(name), priority = 5) as s:
        exit_code, out, err = call_and_wait(bin_file, cwd=base_dir)
        window = sublime.active_window()
        if not window:
            return
        if exit_code == 0:
            sublime.set_timeout(lambda: write_output(window, out, base_dir), 0)
        else:
            s.fail()
            sublime.set_timeout(lambda: write_output(window, err, base_dir), 0)


def write_output(window, text, base_dir):
    "Write text to Sublime's output panel."
    output_view = window.get_output_panel(OUTPUT_PANEL_NAME)
    output_view.set_read_only(False)
    # Configure Sublime's error message parsing:
    output_view.settings().set("result_base_dir", base_dir)
    # Write to the output buffer:
    output_view.run_command('sublime_haskell_output_text', {
        'text': text })
    # Set the selection to the beginning of the view so that "next result" works:
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0))
    output_view.set_read_only(True)
    # Show the results panel:
    window.run_command('show_panel', {'panel': 'output.' + OUTPUT_PANEL_NAME})


def hide_output(window):
    window.run_command('hide_panel', {'panel': 'output.' + OUTPUT_PANEL_NAME})

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
