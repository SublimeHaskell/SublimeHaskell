# -*- coding: UTF-8 -*-
# pylinee: disable=fixme

import os
import os.path
import threading

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.hsdev.agent as hsdev

OUTPUT_PANEL_NAME = "haskell_run_output"

BUILD_TOOL = {
    'cabal': {'command': 'cabal', 'name': 'cabal'},
    'cabal-new-build': {'command': 'cabal', 'name': 'cabal (new build)'},
    'stack': {'command': 'stack', 'name': 'stack'}
}


def same_steps(steps):
    return {
        'cabal': steps,
        'cabal-new-build': steps,
        'stack': steps
    }

BUILD_TOOL_CONFIG = {
    'clean': {
        'message': 'Cleaning',
        'steps': same_steps([['clean']])
    },
    'configure': {
        'message': 'Configuring',
        'steps': {
            'cabal':           [['configure', '--enable-tests']],
            'cabal-new-build': [['new-configure', '--enable-tests']],
            'stack':           []
        }
    },
    'build': {
        'message': 'Building',
        'steps': {
            'cabal':           [['build']],
            'cabal-new-build': [['new-build']],
            'stack':           [['build']]
        }
    },
    'typecheck': {
        'message': 'Checking',
        'steps': {
            'cabal':           [['build', '--ghc-options=-c']],
            'cabal-new-build': [['new-build', '--ghc-options=-c']],
            'stack':           [['build', '--ghc-options=-c']]
        }
    },
    # Commands with warnings:
    # Run fast, incremental build first. Then build everything with -Wall and -fno-code
    # If the incremental build fails, the second step is not executed.
    'build_then_warnings': {
        'message': 'Building',
        'steps': {
            'cabal':           [['build'], ['build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
            'cabal-new-build': [['new-build'], ['new-build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
            'stack':           [['build']]
        }
    },
    'typecheck_then_warnings': {
        'message': 'Checking',
        'steps': {
            'cabal':           [['build', '--ghc-options=-c'], ['build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
            'cabal-new-build': [['new-build', '--ghc-options=-c'],
                                ['new-build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
            'stack':           [['build']]
        }
    },

    'rebuild': {
        'message': 'Rebuilding',
        'steps': {
            'cabal':           [['clean'], ['configure', '--enable-tests'], ['build']],
            'cabal-new-build': [['clean'], ['new-configure', '--enable-tests'], ['new-build']],
            'stack':           [['clean'], ['build']]
        }
    },
    'install': {
        'message': 'Installing',
        'steps': {
            'cabal':           [['install', '--enable-tests']],
            'cabal-new-build': [['install', '--enable-tests']],
            'stack':           [['install']]
        }
    },
    'test': {
        'message': 'Testing',
        'steps': same_steps([['test']])
    }
}

# GLOBAL STATE

# Contains names of projects currently being built.
# To be updated only from the UI thread.
PROJECTS_BEING_BUILT = set()


# Base command
class SublimeHaskellBaseCommand(Common.SublimeHaskellWindowCommand):
    def __init__(self, window):
        super().__init__(window)

    def build(self, command, filter_project=None):
        self.select_project(lambda n, d: run_build(self.window.active_view(), n, d, BUILD_TOOL_CONFIG[command]),
                            filter_project)

    def is_enabled(self):
        return Common.is_enabled_haskell_command(None, False)

    def is_visible(self):
        return Common.is_enabled_haskell_command(None, False)


    # Select project from list
    # on_selected accepts name of project and directory of project
    # filter_project accepts name of project and project-info as it appears in AutoCompletion object
    #   and returns whether this project must appear in selection list
    def select_project(self, on_selected, filter_project):
        projs = [(name, info) for (name, info) in get_projects().items() if not filter_project or filter_project(name, info)]

        def run_selected(psel):
            on_selected(psel[0], psel[1]['path'])

        if len(projs) == 0:
            Common.show_status_message("No projects found, did you add a '.cabal' file?", is_ok=False, priority=5)
            return
        if len(projs) == 1:  # There's only one project, build it
            run_selected(projs[0])
            return

        _, cabal_project_name = Common.get_cabal_project_dir_and_name_of_view(self.window.active_view())
        Logging.log('Current project: {0}'.format(cabal_project_name))

        # Sort by name
        projs.sort(key=lambda p: p[0])

        current_project_idx = next((i for i, p in enumerate(projs) if p[0] == cabal_project_name), -1)

        def on_done(idx):
            if idx != -1:
                run_selected(projs[idx])

        self.window.show_quick_panel(list(map(lambda m: [m[0], m[1]['path']], projs)), on_done, 0, current_project_idx)


def is_stack_project(project_dir):
    """Search for stack.yaml in parent directories"""
    return Common.find_file_in_parent_dir(project_dir, "stack.yaml") is not None


# Get stack dist path
def stack_dist_path(project_dir):
    exit_code, out, _err = ProcHelper.ProcHelper.run_process(['stack', 'path'], cwd=project_dir)
    if exit_code == 0:
        distdirs = [d for d in out.splitlines() if d.startswith('dist-dir: ')]
        if len(distdirs) > 0:
            dist_dir = distdirs[0][10:]
            return os.path.join(project_dir, dist_dir)


# Retrieve projects as dictionary that refers to this app instance
def get_projects():
    if hsdev.agent_connected():
        folders = sublime.active_window().folders()
        view_files = [v.file_name() for v in sublime.active_window().views()
                      if (Common.is_haskell_source(v) or Common.is_cabal_source(v)) and v.file_name()
                     ]

        def npath(path):
            return os.path.normcase(os.path.normpath(path))

        def childof(path, prefix):
            return npath(path).startswith(npath(prefix))

        return dict((info['name'], info) for info in filter(
            lambda p: any([childof(p['path'], f) for f in folders]) or any([childof(src, p['path']) for src in view_files]),
            (hsdev.client.list_projects() or [])))
    else:
        folder_files = [src for f in sublime.active_window().folders() for src in Common.list_files_in_dir_recursively(f) \
                        if os.path.splitext(src)[1] in [".hs", ".cabal"]
                       ]
        view_files = [v.file_name() for v in sublime.active_window().views()
                      if (Common.is_haskell_source(v) or Common.is_cabal_source(v)) and v.file_name()
                     ]
        src_files = list(map(lambda p: os.path.normcase(os.path.normpath(p)), folder_files + view_files))
        active_projects = []
        while src_files:
            src = src_files.pop()
            proj_dir, proj_name = Common.get_cabal_project_dir_and_name_of_file(src)
            if proj_dir:
                active_projects.append((proj_name, proj_dir))
                src_files = [f for f in src_files if not f.startswith(proj_dir)]

        return dict((name, {'name': name, 'path': path}) for name, path in active_projects)


def run_build(view, project_name, project_dir, config):
    # Don't build if a build is already running for this project
    # We compare the project_name for simplicity (projects with same
    # names are of course possible, but unlikely, so we let them wait)
    if project_name in PROJECTS_BEING_BUILT:
        Logging.log("Waiting for build action on '%s' to complete." % project_name, Logging.LOG_WARNING)
        Common.show_status_message('Already building %s' % project_name, is_ok=False, priority=5)
        return

    # Set project as building
    PROJECTS_BEING_BUILT.add(project_name)

    build_tool_name = Settings.PLUGIN.haskell_build_tool
    if build_tool_name == 'stack' and not is_stack_project(project_dir):  # rollback to cabal
        build_tool_name = 'cabal'

    tool = BUILD_TOOL[build_tool_name]

    # Title of tool: Cabal, Stack
    tool_title = tool['name']
    # Title of action: Cleaning, Building, etc.
    action_title = config['message']
    # Tool name: cabal
    tool_name = tool['command']
    # Tool arguments (commands): build, clean, etc.
    tool_steps = config['steps'][build_tool_name]

    # Config override
    override_config = Settings.get_project_setting(view, 'stack_config_file')

    override_args = []
    if override_config:
        override_args = ['--stack-yaml', override_config]
    # Assemble command lines to run (possibly multiple steps)
    commands = [[tool_name] + step + override_args for step in tool_steps]

    Logging.log('running build commands: {0}'.format(commands), Logging.LOG_TRACE)

    def done_callback():
        # Set project as done being built so that it can be built again
        PROJECTS_BEING_BUILT.remove(project_name)

    # Run them
    ParseOutput.run_chain_build_thread(view,
                                       project_dir,
                                       '{0} {1} with {2}'.format(action_title, project_name, tool_title),
                                       commands,
                                       on_done=done_callback)


# Default build system (cabal or cabal-dev)

# class SublimeHaskellCleanCommand(SublimeHaskellBaseCommand):
#     def run(self):
#         self.build('clean')


# class SublimeHaskellConfigureCommand(SublimeHaskellBaseCommand):
#     def run(self):
#         self.build('configure')


class SublimeHaskellBuildCommand(SublimeHaskellBaseCommand):
    def run(self, task='build'):
        self.build(task)


class SublimeHaskellTypecheckCommand(SublimeHaskellBaseCommand):
    def run(self):
        self.build('typecheck_then_warnings')


# class SublimeHaskellRebuildCommand(SublimeHaskellBaseCommand):
#     def run(self):
#         self.build('rebuild')


# class SublimeHaskellInstallCommand(SublimeHaskellBaseCommand):
#     def run(self):
#         self.build('install')


# class SublimeHaskellTestCommand(SublimeHaskellBaseCommand):
#     def run(self):
#         def has_tests(_, info):
#             return len(info['description']['tests']) > 0

#         self.build('test', filter_project=has_tests)


# Auto build current project
class SublimeHaskellBuildAutoCommand(SublimeHaskellBaseCommand):
    def run(self):
        current_project_dir, current_project_name = Common.get_cabal_project_dir_and_name_of_view(self.window.active_view())
        if current_project_name and current_project_dir:
            build_mode = Settings.PLUGIN.auto_build_mode
            # run_tests = Settings.PLUGIN.auto_run_tests

            build_command = {
                'normal': 'build',
                'normal-then-warnings': 'build_then_warnings',
                'typecheck': 'typecheck',
                'typecheck-then-warnings': 'typecheck_then_warnings',
            }.get(build_mode)

            if not build_command:
                Common.output_error(self.window, "SublimeHaskell: invalid auto_build_mode '%s'" % build_mode)

            config = BUILD_TOOL_CONFIG[build_command]

            # TODO: Auto run tests

            # if run_tests:
            #     has_tests = False

            #     projects = get_projects()

            #     if current_project_name in projects and 'description' in projects[current_project_name]:
            #         has_tests = len(projects[current_project_name]['description']['tests']) > 0

            #     if has_tests:
            #         config['steps'].extend(cabal_config['test']['steps'])

            run_build(self.window.active_view(), current_project_name, current_project_dir, config)


def project_dist_path(project_dir):
    if is_stack_project(project_dir):
        return stack_dist_path(project_dir)
    else:
        return os.path.join(project_dir, 'dist')


class SublimeHaskellRunCommand(SublimeHaskellBaseCommand):
    def __init__(self, view):
        super().__init__(view)
        self.executables = []

    def run(self):
        self.executables = []
        projs = []
        projects = get_projects()
        for proj, info in projects.items():
            if 'description' in info:
                for exes in info['description']['executables']:
                    projs.append((proj + ": " + exes['name'], {
                        'dir': info['path'],
                        'dist': project_dist_path(info['path']),
                        'name': exes['name']
                        }))

        # Nothing to run
        if len(projs) == 0:
            Common.sublime_status_message('Nothing to run')
            return

        _, cabal_project_name = Common.get_cabal_project_dir_and_name_of_view(self.window.active_view())

        # Show current project first
        projs.sort(key=lambda s: (not s[0].startswith(cabal_project_name), s[0]))

        self.executables = list(map(lambda m: m[1], projs))
        self.window.show_quick_panel(list(map(lambda m: m[0], projs)), self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        selected = self.executables[idx]
        name = selected['name']
        base_dir = selected['dir']
        bin_file = os.path.join(selected['dist'], 'build', name, name)

        hide_output(self.window)

        # Run in thread
        threading.Thread(target=run_binary, args=(name, bin_file, base_dir)).start()


def run_binary(name, bin_file, base_dir):
    with Common.status_message_process('Running {0}'.format(name), priority=5) as smsg:
        exit_code, out, err = ProcHelper.ProcHelper.run_process([bin_file], cwd=base_dir)
        window = sublime.active_window()
        if not window:
            return
        if exit_code == 0:
            smsg.ok()
            sublime.set_timeout(lambda: write_output(window, out, base_dir), 0)
        else:
            smsg.fail()
            sublime.set_timeout(lambda: write_output(window, err, base_dir), 0)


def write_output(window, text, base_dir):
    "Write text to Sublime's output panel."
    output_view = Common.output_panel(window, text, panel_name=OUTPUT_PANEL_NAME, panel_display=True)
    output_view.settings().set("result_base_dir", base_dir)


def hide_output(window):
    window.run_command('hide_panel', {'panel': 'output.' + OUTPUT_PANEL_NAME})
