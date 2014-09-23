import os
import os.path
import sublime
import sublime_plugin
import copy
from threading import Thread

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    from parseoutput import run_chain_build_thread
    import autocomplete
    import hsdev
else:
    from SublimeHaskell.sublime_haskell_common import *
    from SublimeHaskell.parseoutput import run_chain_build_thread
    import SublimeHaskell.autocomplete as autocomplete
    import SublimeHaskell.hsdev as hsdev

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
class SublimeHaskellBaseCommand(SublimeHaskellWindowCommand):

    def build(self, command, use_cabal_sandbox=None, filter_project = None):
        select_project(
            self.window,
            lambda n, d: run_build(self.window.active_view(), n, d, cabal_config[command], use_cabal_sandbox),
            filter_project = filter_project)

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)

# Retrieve projects as dictionary that refers to this app instance
def get_projects():
    if autocomplete.hsdev_agent_connected():
        folders = sublime.active_window().folders()
        view_files = [v.file_name() for v in sublime.active_window().views() if is_haskell_source(v) or is_cabal_source(v)]

        def npath(p):
            return os.path.normcase(os.path.normpath(p))

        def childof(c, p):
            return npath(c).startswith(npath(p))

        return dict((info['name'], info) for info in filter(
            lambda p: any([childof(p['path'], f) for f in folders]) or any([childof(src, p['path']) for src in view_files]),
            (autocomplete.hsdev_client.list_projects() or [])))
    else:
        folder_files = [src for f in sublime.active_window().folders() for src in autocomplete.list_files_in_dir_recursively(f) if os.path.splitext(src)[1] in [".hs", ".cabal"]]
        view_files = [v.file_name() for v in sublime.active_window().views() if is_haskell_source(v) or is_cabal_source(v)]
        src_files = list(map(lambda p: os.path.normcase(os.path.normpath(p)), folder_files + view_files))
        active_projects = []
        while src_files:
            src = src_files.pop()
            proj_dir, proj_name = get_cabal_project_dir_and_name_of_file(src)
            if proj_dir:
                active_projects.append(proj_name, proj_dir)
                src_files = [f for f in src_files if not f.startswith(proj_dir)]

        return active_projects

# Select project from list
# on_selected accepts name of project and directory of project
# filter_project accepts name of project and project-info as it appears in AutoCompletion object
#   and returns whether this project must appear in selection list
def select_project(window, on_selected, filter_project = None):
    ps = [(name, info) for (name, info) in get_projects().items() if not filter_project or filter_project(name, info)]

    def run_selected(psel):
        on_selected(psel[0], psel[1]['path'])

    if len(ps) == 0:
        return
    if len(ps) == 1:  # There's only one project, build it
        run_selected(ps[0])
        return

    cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(window.active_view())
    log('Current project: {0}'.format(cabal_project_name))

    # Sort by name
    ps.sort(key = lambda p: p[0])

    current_project_idx = next((i for i, p in enumerate(ps) if p[0] == cabal_project_name), -1)

    def on_done(idx):
        if idx != -1:
            run_selected(ps[idx])

    window.show_quick_panel(list(map(lambda m: [m[0], m[1]['path']], ps)), on_done, 0, current_project_idx)


def run_build(view, project_name, project_dir, config, use_cabal_sandbox=None):
    global projects_being_built

    # Don't build if a build is already running for this project
    # We compare the project_name for simplicity (projects with same
    # names are of course possible, but unlikely, so we let them wait)
    if project_name in projects_being_built:
        log("Not building '%s' because it is already being built" % project_name, log_warning)
        sublime_status_message('Already building %s' % project_name)
        return
    # Set project as building
    projects_being_built.add(project_name)

    # Run cabal or cabal-dev
    if use_cabal_sandbox is None:
        use_cabal_sandbox = get_setting_async('use_cabal_sandbox')

    tool = cabal_tool[use_cabal_sandbox]

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

    log('running build commands: {0}'.format(commands), log_trace)

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


class SublimeHaskellSwitchCabalSandboxCommand(SublimeHaskellWindowCommand):
    def run(self):
        use_cabal_sandbox = get_setting('use_cabal_sandbox')
        sandbox = get_setting('cabal_sandbox')
        sandboxes = get_setting('cabal_sandbox_list')

        sandboxes.append(sandbox)
        sandboxes = list(set(sandboxes))
        set_setting('cabal_sandbox_list', sandboxes)

        # No sandboxes
        if len(sandboxes) == 0:
            sublime_status_message('There is nothing to switch to')
            self.switch_cabal('cabal')
            return

        # One sandbox, just switch
        if len(sandboxes) == 1:
            self.switch_cabal('cabal' if use_cabal_sandbox else sandbox)
            return

        # Many sandboxes, show list
        self.sorted_sands = sandboxes
        # Move previously used sandbox (or cabal) on top
        self.sorted_sands.remove(sandbox)
        if use_cabal_sandbox:
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
            self.switch_cabal('cabal')
        else:
            self.switch_cabal(selected)

    def switch_cabal(self, new_cabal):
        old_cabal = current_cabal()

        if new_cabal == old_cabal:
            return

        if new_cabal == 'cabal':
            set_setting('use_cabal_sandbox', False)
        else:
            set_setting('cabal_sandbox', new_cabal)
            set_setting('use_cabal_sandbox', True)

        save_settings()

        sublime_status_message('Switched to ' + new_cabal)


# Default build system (cabal or cabal-dev)

class SublimeHaskellCleanCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.build('clean')


class SublimeHaskellConfigureCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.build('configure')


class SublimeHaskellBuildCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.build('build_then_warnings')


class SublimeHaskellTypecheckCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.build('typecheck_then_warnings')


class SublimeHaskellRebuildCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.build('rebuild')


class SublimeHaskellInstallCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.build('install')

class SublimeHaskellTestCommand(SublimeHaskellBaseCommand):
    def run(self):
        def has_tests(name, info):
            return len(info['tests']) > 0

        self.build('test', filter_project = has_tests)

# Auto build current project
class SublimeHaskellBuildAutoCommand(SublimeHaskellBaseCommand):
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

                projects = get_projects()

                if current_project_name in projects:
                    has_tests = len(projects[current_project_name]['description']['tests']) > 0

                if has_tests:
                    config['steps'].extend(cabal_config['test']['steps'])

            run_build(self.window.active_view(), current_project_name, current_project_dir, config, None)


class SublimeHaskellRunCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.executables = []
        ps = []
        projects = get_projects()
        for p, info in projects.items():
            for e in info['description']['executables']:
                ps.append((p + ": " + e['name'], {
                    'dir': info['path'],
                    'name': info['name']
                    }))

        # Nothing to run
        if len(ps) == 0:
            sublime_status_message('Nothing to run')
            return

        cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(self.window.active_view())

        # Show current project first
        ps.sort(key = lambda s: (not s[0].startswith(cabal_project_name), s[0]))

        self.executables = list(map(lambda m: m[1], ps))
        self.window.show_quick_panel(list(map(lambda m: m[0], ps)), self.on_done)

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
    output_view = output_panel(window, text, panel_name = OUTPUT_PANEL_NAME)
    output_view.settings().set("result_base_dir", base_dir)


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
