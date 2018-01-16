# -*- coding: UTF-8 -*-
# pylinee: disable=fixme

import os
import os.path
import pprint
import shlex
import threading

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.internals.backend_mgr as BackendMgr
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.sublime_haskell_common as Common


OUTPUT_PANEL_NAME = "haskell_run_output"
BUILD_LOG_PANEL_NAME = 'sublime_haskell_build_log_panel'


def cabal_new_clean(project_dir):
    '''Internal command that does the "cabal new-"-style 'clean' command (until it is actually implemented.)
    '''
    def remove_subdir_recursively(subdir):
        subdir = Utils.normalize_path(os.path.join(project_dir, subdir))
        if os.path.exists(subdir):
            if os.path.isdir(subdir):
                for dirpath, _dirnames, filenames in os.walk(subdir, topdown=False):
                    try:
                        for fname in filenames:
                            os.remove(os.path.join(dirpath, fname))
                        os.rmdir(dirpath)
                    except OSError as ex:
                        diag_feedback.append(str(ex))
                        print(ex)
            else:
                os.remove(subdir)

    cabal_project_local = Utils.normalize_path(os.path.join(project_dir, 'cabal.project.local'))

    diag_feedback = []

    for subd in ['dist-newstyle', 'dist']:
        remove_subdir_recursively(subd)
    if os.path.exists(cabal_project_local):
        os.remove(cabal_project_local)

    return (0 if not diag_feedback else 1, '\n'.join(diag_feedback))


class SublimeHaskellBuildCommand(CommandWin.SublimeHaskellWindowCommand):
    '''Base class for the builder commands (build, configure, test, install, ...).
    '''

    PROJECTS_BEING_BUILT = set()
    '''Projects that are currently being built, used to ensure that the same project isn't being
    built multiple times.

    NOTE: Should only be updated via the UI thread. Since this is the base class for most project-build
    commands, that's almost automagic.
    '''


    BUILD_TOOL = {
        'cabal':           {'command': 'cabal', 'name': 'cabal'},
        'cabal-new-build': {'command': 'cabal', 'name': 'cabal (Nix-local/new build)'},
        'stack':           {'command': 'stack', 'name': 'stack'}
    }
    '''Command and information associated with the various build tools.
    '''


    BUILD_TOOL_CONFIG = {
        'clean': {
            'message': 'Cleaning',
            'steps': {
                'cabal':           [['clean']],
                'cabal-new-build': [cabal_new_clean],
                'stack':           [['clean']]
            }
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
                'cabal':           [['build'],
                                    ['build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
                'cabal-new-build': [['new-build'],
                                    ['new-build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
                'stack':           [['build']]
            }
        },
        'typecheck_then_warnings': {
            'message': 'Checking',
            'steps': {
                'cabal':           [['build', '--ghc-options=-c'],
                                    ['build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
                'cabal-new-build': [['new-build', '--ghc-options=-c'],
                                    ['new-build', '-v0', '--ghc-options=-fforce-recomp -fno-code']],
                'stack':           [['build']]
            }
        },

        'rebuild': {
            'message': 'Rebuilding',
            'steps': {
                'cabal':           [['clean'],
                                    ['configure', '--enable-tests'],
                                    ['build']],
                'cabal-new-build': [cabal_new_clean,
                                    ['new-configure', '--enable-tests'],
                                    ['new-build']],
                'stack':           [['clean'],
                                    ['build']]
            }
        },
        'install': {
            'message': 'Installing',
            'steps': {
                'cabal':           [['install', '--enable-tests']],
                'cabal-new-build': [],
                'stack':           [['install']]
            }
        },
        'test': {
            'message': 'Testing',
            'steps': {
                'cabal':           [['test']],
                'cabal-new-build': [['new-test']],
                'stack':           [['test']]
            }
        },
        'freeze': {
            'message': 'Dependency freeze',
            'steps': {
                'cabal':           [['freeze']],
                'cabal-new-build': [['new-freeze']]
            }
        },
        'bench': {
            'message': 'Execute benchmarks',
            'steps': {
                'cabal':            [['bench']],
                'cabal-new-build':  [['new-bench']],
                'stack':            [['bench']]
            }
        }
    }


    ## Pylinter complains that this is a useless super delegation. Uncomment when instance attributes are added.
    # def __init__(self, window):
    #     super().__init__(window)

    def run(self, **args):
        task = args.get('task', 'build')
        self.build(task)

    def build(self, command, filter_project=None):
        self.select_project(lambda n, d: self.run_build(self.window.active_view(), n, d, self.BUILD_TOOL_CONFIG[command]),
                            filter_project)

    def is_enabled(self):
        return Common.is_enabled_haskell_command(None, False)

    def is_visible(self):
        return Common.is_enabled_haskell_command(None, False)


    def select_project(self, on_selected, filter_project):
        '''Select a project from a generated project list. Execution flow continues into the :py:function:`on_selected`
        function with the project's name and the project's base directory. The :py:function:`filter_project` filters
        projects before they are shown (see :py:method:`get_projects`.)
        '''
        projs = [(name, info) for (name, info) in self.get_projects().items()
                 if not filter_project or filter_project(name, info)]

        def run_selected(psel):
            on_selected(psel[0], psel[1]['path'])

        if not projs:
            Common.sublime_status_message("No active projects found.")
        elif len(projs) == 1:
            # There's only one project, build it
            run_selected(projs[0])
        else:
            _, cabal_project_name = Common.locate_cabal_project_from_view(self.window.active_view())
            Logging.log('Current project: {0}'.format(cabal_project_name))

            # Sort by name
            projs.sort(key=lambda p: p[0])

            current_project_idx = next((i for i, p in enumerate(projs) if p[0] == cabal_project_name), -1)

            def on_done(idx):
                if idx != -1:
                    run_selected(projs[idx])

            self.window.show_quick_panel(list(map(lambda m: [m[0], m[1]['path']], projs)), on_done, 0, current_project_idx)


    # Retrieve projects as dictionary that refers to this app instance
    def get_projects(self):
        folders = sublime.active_window().folders()
        view_files = [v.file_name() for v in sublime.active_window().views()
                      if (Common.view_is_haskell_source(v) or Common.view_is_cabal_source(v)) and v.file_name()]

        def childof(path, prefix):
            return Utils.normalize_path(path).startswith(Utils.normalize_path(prefix))

        def relevant_project(proj):
            return any([childof(proj['path'], f) for f in folders]) or any([childof(src, proj['path']) for src in view_files])

        projects = BackendMgr.active_backend().list_projects() or []
        return dict((info['name'], info) for info in projects if relevant_project(info))


    def run_build(self, view, project_name, project_dir, config):
        # Don't build if a build is already running for this project
        # We compare the project_name for simplicity (projects with same
        # names are of course possible, but unlikely, so we let them wait)
        if project_name in self.PROJECTS_BEING_BUILT:
            Logging.log("Waiting for build action on '%s' to complete." % project_name, Logging.LOG_WARNING)
            Common.sublime_status_message('Already building {0}'.format(project_name))
            return

        # Set project as building
        self.PROJECTS_BEING_BUILT.add(project_name)

        Logging.log('project build tool: {0}'.format(Settings.get_project_setting(view, 'haskell_build_tool')),
                    Logging.LOG_DEBUG)
        Logging.log('settings build tool: {0}'.format(Settings.PLUGIN.haskell_build_tool), Logging.LOG_DEBUG)

        build_tool_name = Settings.get_project_setting(view, 'haskell_build_tool', Settings.PLUGIN.haskell_build_tool)
        if build_tool_name == 'stack' and not self.is_stack_project(project_dir):  # rollback to cabal
            build_tool_name = 'cabal'

        tool = self.BUILD_TOOL[build_tool_name]

        # Title of tool: Cabal, Stack
        tool_title = tool['name']
        # Title of action: Cleaning, Building, etc.
        action_title = config['message']
        # Tool name: cabal
        tool_name = tool['command']
        # Tool arguments (commands): build, clean, etc.
        tool_steps = config['steps'][build_tool_name]

        # Config override
        override_config = Settings.get_project_setting(view, 'active_stack_config')

        override_args = []
        if override_config:
            override_args = ['--stack-yaml', override_config]
        # Assemble command lines to run (possibly multiple steps)
        commands = [[tool_name] + override_args + step if isinstance(step, list) else step for step in tool_steps]

        Logging.log('running build commands: {0}'.format(commands), Logging.LOG_TRACE)

        # Run them
        ## banner = '{0} {1} with {2}\ncommands:\n{3}'.format(action_title, project_name, tool_title, commands)
        banner = '{0} {1} with {2}'.format(action_title, project_name, tool_title)
        Logging.log(banner, Logging.LOG_DEBUG)
        Utils.run_async('wait_for_chain_to_complete', self.wait_for_chain_to_complete, view, project_name, project_dir,
                        banner, commands)


    def wait_for_chain_to_complete(self, view, cabal_project_name, cabal_project_dir, banner, cmds):
        '''Chains several commands, wait for them to complete, then parse and display
        the resulting errors.'''

        # First hide error panel to show that something is going on
        sublime.set_timeout(lambda: hide_output(view), 0)

        # run and wait commands, fail on first fail
        # exit_code has scope outside of the loop
        collected_out = []
        exit_code = 0
        output_log = Common.output_panel(view.window(), '',
                                         panel_name=BUILD_LOG_PANEL_NAME,
                                         panel_display=Settings.PLUGIN.show_output_window)
        for cmd in cmds:
            if isinstance(cmd, list):
                Common.output_text(output_log, ' '.join(cmd) + '\u2026\n')

                # Don't tie stderr to stdout, since we're interested in the error messages
                out = OutputCollector.OutputCollector(output_log, cmd, cwd=cabal_project_dir)
                exit_code, cmd_out = out.wait()
            elif callable(cmd):
                Common.output_text(output_log, 'Function/method {0}\n'.format(cmd.__name__))
                exit_code, cmd_out = cmd(cabal_project_dir)
            else:
                # Clearly something not a list or callable:
                pass

            collected_out.append(cmd_out)
            # Bail if the command failed...
            if exit_code != 0:
                break

        if collected_out or exit_code == 0:
            # We're going to show the errors in the output panel...
            Common.hide_panel(view.window(), panel_name=BUILD_LOG_PANEL_NAME)

        # Notify UI thread that commands are done
        self.PROJECTS_BEING_BUILT.remove(cabal_project_name)
        ParseOutput.MARKER_MANAGER.mark_compiler_output(view, banner, cabal_project_dir, ''.join(collected_out), exit_code)


    def project_dist_path(self, project_dir):
        return self.stack_dist_path(project_dir) \
            if self.is_stack_project(project_dir) \
            else os.path.join(project_dir, 'dist')


    def is_stack_project(self, project_dir):
        """Search for stack.yaml in parent directories"""
        return Common.find_file_in_parent_dir(project_dir, "stack.yaml") is not None


    # Get stack dist path
    def stack_dist_path(self, project_dir):
        exit_code, out, _err = ProcHelper.ProcHelper.run_process(['stack', 'path'], cwd=project_dir)
        if exit_code == 0:
            distdirs = [d for d in out.splitlines() if d.startswith('dist-dir: ')]
            if distdirs:
                dist_dir = distdirs[0][10:]
                return os.path.join(project_dir, dist_dir)


class SublimeHaskellBuildCabalOnlyCommand(SublimeHaskellBuildCommand):
    '''SublimeHaskell commands that are only valid if the project builder is cabal or 'cabal new-build'.
    '''
    def is_enabled(self):
        project_builder = Settings.get_project_setting(self.window.active_view(), 'haskell_build_tool',
                                                       Settings.PLUGIN.haskell_build_tool)
        return super().is_enabled() and project_builder and project_builder in ['cabal', 'cabal-new-build']

    def is_visible(self):
        project_builder = Settings.get_project_setting(self.window.active_view(), 'haskell_build_tool',
                                                       Settings.PLUGIN.haskell_build_tool)
        return super().is_visible and project_builder and project_builder in ['cabal', 'cabal-new-build']


# Auto build current project
class SublimeHaskellBuildAutoCommand(SublimeHaskellBuildCommand):
    MODE_BUILD_COMMAND = {
        'normal': 'build',
        'normal-then-warnings': 'build_then_warnings',
        'normal-then-tests': 'build',
        'normal-then-bench': 'build',
        'typecheck': 'typecheck',
        'typecheck-then-warnings': 'typecheck_then_warnings',
    }

    def run(self, **_args):
        current_project_dir, current_project_name = Common.locate_cabal_project_from_view(self.window.active_view())
        if current_project_name and current_project_dir:
            build_mode = Settings.get_project_setting(self.window.active_view(), 'auto_build_mode',
                                                      Settings.PLUGIN.auto_build_mode)

            build_command = self.MODE_BUILD_COMMAND.get(build_mode)

            if not build_command or build_command not in self.MODE_BUILD_COMMAND:
                Common.output_error(self.window, "SublimeHaskell: invalid auto_build_mode '%s'" % build_mode)

            # Duplicate the dictionary corresponding to the build command. We might modify it later.
            config = dict(self.BUILD_TOOL_CONFIG[build_command])
            addl_config = None

            if build_mode.endswith('-then-tests'):
                has_tests = False
                projects = self.get_projects()

                if current_project_name in projects and 'description' in projects[current_project_name]:
                    has_tests = projects[current_project_name]['description'].get('tests') is not None

                if has_tests:
                    addl_config = 'test'
            elif build_mode.endswith('-then-bench'):
                addl_config = 'bench'

            if addl_config is not None:
                for tool, steps in self.BUILD_TOOL_CONFIG[addl_config]['steps'].items():
                    config['steps'][tool].extend(steps)

            Logging.log('auto build: final config:\n{0}'.format(pprint.pformat(config)))

            self.run_build(self.window.active_view(), current_project_name, current_project_dir, config)


class SublimeHaskellRunCommand(SublimeHaskellBuildCommand):
    def __init__(self, window):
        super().__init__(window)
        self.executables = []
        self.exec_name = ''
        self.exec_base_dir = ''

    def run(self, **_args):
        self.executables = []
        projs = []
        projects = self.get_projects()
        for proj, info in projects.items():
            if 'description' in info:
                for exes in info['description']['executables']:
                    projs.append((proj + ': ' + exes['name'], {'dir': info['path'], 'name': exes['name']}))
        print('SublimeHaskellRunCommand: projects {0}'.format(projects))

        if not projs:
            Common.sublime_status_message('No project or nothing to run')
        elif len(projs) == 1:
            # One project
            proj_info = projs[0][1]
            self.exec_name = proj_info['name']
            self.exec_base_dir = proj_info['dir']
            self.prompt_prog_args()
        else:
            # Multiple choices
            _, cabal_project_name = Common.locate_cabal_project_from_view(self.window.active_view())

            # Show current project first
            projs.sort(key=lambda s: (not s[0].startswith(cabal_project_name + ': '), s[0]))

            self.executables = [p[1] for p in projs]
            self.window.show_quick_panel([p[0] for p in projs], self.on_project_selected)

    def prompt_prog_args(self):
        args = ''
        view = self.window.active_view()
        if view:
            run_args = (view.settings() or {}).get('subhask_run_args', {})
            args = run_args.get(self.exec_name, '')

        self.window.show_input_panel('Program arguments (shlex)', args, self.on_program_args, None, None)

    def on_project_selected(self, idx):
        if idx > -1:
            selected = self.executables[idx]
            self.exec_name = selected['name']
            self.exec_base_dir = selected['dir']
            self.prompt_prog_args()

    def on_program_args(self, args):
        view = self.window.active_view()
        view_settings = view.settings()
        run_args = (view_settings or {}).get('subhask_run_args', {})
        run_args[self.exec_name] = args
        view_settings.set('subhask_run_args', run_args)
        project_builder = Settings.get_project_setting(view, 'haskell_build_tool', Settings.PLUGIN.haskell_build_tool)
        cmd_list = ProcHelper.exec_wrapper_cmd(project_builder, [self.exec_name] + shlex.split(args))

        hide_output(self.window)
        outview = Common.output_panel(self.window, panel_name=OUTPUT_PANEL_NAME)

        pretty_cmdargs = 'Running \'{0}\' in {1}'.format(' '.join(cmd_list), self.exec_base_dir)
        outview.run_command('insert', {'characters': '{0}\n{1}\n'.format(pretty_cmdargs, '-' * len(pretty_cmdargs))})
        self.ExecRunner(outview, cmd_list, self.exec_base_dir).start()


    class ExecRunner(threading.Thread):
        def __init__(self, panel, cmdargs, install_dir):
            super().__init__()
            self.the_proc = OutputCollector.OutputCollector(panel, cmdargs, tie_stderr=True, cwd=install_dir)

        def run(self):
            self.the_proc.wait()


def hide_output(window):
    window.run_command('hide_panel', {'panel': 'output.' + OUTPUT_PANEL_NAME})
