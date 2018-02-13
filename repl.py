# -*- coding: UTF-8 -*-

import os
import re

import sublime
import sublime_plugin

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.sublime_haskell_common as Common

HAS_SUBLIME_REPL = True

try:
    import SublimeREPL.sublimerepl as sublimerepl
except ImportError:
    Logging.log('SublimeREPL is not installed, ghci/repl commands disabled', Logging.LOG_INFO)
    HAS_SUBLIME_REPL = False


# Not used. Keep it around just in case it becomes useful in the future because we need to interact with
# the REPL directly.
#
# def run_repl_command(repl, repl_cmd):
#     repl.write("{0}\n".format(repl_cmd))
#     repl.repl.write("{0}\n".format(repl_cmd))


class SublimeHaskellAutocompleteRepl(sublime_plugin.EventListener):
    # Uncomment if attributes are added. Otherwise, it's a useless superclass call.
    #
    # def __init__(self):
    #     super().__init__()

    GHCI_CMDS = [(":{0}\tghci".format(cmd), ":{0}".format(cmd)) for cmd in [
        '!', '?', 'abandon', 'add', 'back', 'break', 'browse!', 'browse', 'cd', 'cmd', 'complete', 'continue',
        'ctags!', 'ctags', 'def', 'delete', 'edit', 'etags', 'force', 'forward', 'help', 'history', 'info!',
        'info', 'issafe', 'kind!', 'kind', 'list', 'load!', 'load', 'main', 'module', 'print', 'quit',
        'reload!', 'reload', 'run', 'show', 'showi', 'script', 'sprint', 'step', 'steplocal', 'stepmodule',
        'trace', 'type', 'undef']]

    GHCI_SHOW_ARG = [('{0}\tghci show'.format(show), '{0}'.format(show)) for show in [
        'args', 'bindings', 'breaks', 'context', 'editor', 'imports', 'language', 'linker', 'modules', 'packages',
        'paths', 'prog', 'prompt', 'stop']]

    GHCI_SHOWI_ARG = [('{0}\tghci showi'.format(show), '{0}'.format(show)) for show in [
        'language']]

    COMMAND_RE = re.compile(r':(\w+)(\s+(\w+))?')

    def on_query_completions(self, view, _prefix, _locations):
        if not HAS_SUBLIME_REPL or not Common.view_is_haskell_repl(view):
            return []

        compl_flags = sublime.INHIBIT_WORD_COMPLETIONS|sublime.INHIBIT_EXPLICIT_COMPLETIONS
        replv = sublimerepl.manager.repl_view(view)
        command = self.COMMAND_RE.match(replv.user_input)
        if command:
            cmd, arg = command.group(1, 3)
            if cmd == 'show':
                return (self.GHCI_SHOW_ARG, compl_flags)
            elif cmd == 'showi':
                return (self.GHCI_SHOWI_ARG, compl_flags)
            elif cmd and not arg:
                return (self.GHCI_CMDS, compl_flags)

            # Have a command and an argument, but we don't know what to do with the argument...
            return []

        # Kind of a punt -- use word completions based on whatever ST finds in the view's buffer.
        return ([], sublime.INHIBIT_EXPLICIT_COMPLETIONS)


def repl_args(**kwargs):
    ret_args = {'type': 'sublime_haskell',
                'encoding': 'utf8',
                'cmd': ['ghci'],
                'cwd': '$file_path',
                'external_id': 'sublime_haskell_repl',
                "syntax": "Packages/SublimeHaskell/Syntaxes/HaskellRepl.tmLanguage"}
    ret_args.update(kwargs)
    ## SublimeREPL does not like/support these arguments
    ret_args.pop('caption', None)
    ret_args.pop('loaded', None)
    return ret_args


def repl_wrapper_cmd(exec_with, args):
    wrapper = []
    if exec_with == 'cabal':
        wrapper = ['cabal', 'repl']
    elif exec_with == 'cabal-new-build':
        wrapper = ['cabal', 'new-repl']
    elif exec_with == 'stack':
        wrapper = ['stack', 'repl']
    else:
        errmsg = 'repl_wrapper_cmd: Unknown execution prefix \'{0}\''.format(exec_with)
        raise RuntimeError(errmsg)

    return wrapper + args


class SublimeHaskellReplGhciCommand(CommandWin.SublimeHaskellWindowCommand):
    # def __init__(self, window):
    #     super().__init__(window)

    def run(self, **_kwargs):
        opts = Settings.PLUGIN.ghci_opts or []
        self.window.run_command("repl_open", repl_args(cmd=["ghci"] + opts, loaded=None, caption="ghci"))

    def is_enabled(self):
        return HAS_SUBLIME_REPL

    def is_visible(self):
        return True


class SublimeHaskellReplGhciCurrentFileCommand(CommandWin.SublimeHaskellWindowCommand):
    def run(self, **_kwargs):
        view = self.window.active_view()
        if not view:
            Common.sublime_status_message("No file active")
        else:
            opts = Settings.PLUGIN.ghci_opts or []
            self.window.run_command("repl_open", repl_args(cmd=["ghci", "$file"] + opts,
                                                           loaded=view.file_name(),
                                                           caption="ghci: {0}".format(os.path.basename(view.file_name()))))

    def is_enabled(self):
        return HAS_SUBLIME_REPL


class SublimeHaskellReplCabalCommand(CommandWin.SublimeHaskellWindowCommand):
    FILE_NAME_TRANS = str.maketrans({
        '"': '_',
        '*': '_',
        ',': '-',
        '/': '_',
        ':': '-',
        ';': '_',
        '=': '-',
        '?': '-'
        '[': '_',
        '\\': '_',
        ']': '_',
        '|': '_',
        })

    def __init__(self, window):
        super().__init__(window)
        self.view = None
        self.project_name = None
        self.project_dir = None
        self.targets = []
        self.args = {}

    def run(self, **_kwargs):
        self.view = self.window.active_view()
        if self.view:
            self.targets = []
            self.args = {}

            project_dir, project_name = Common.locate_cabal_project_from_view(self.view)
            Logging.log('repl: project_dir {0} project_name {1}'.format(project_dir, project_name), Logging.LOG_DEBUG)
            if project_dir:
                proj_info = BackendManager.active_backend().project(project_name)
                self.project_name = project_name
                self.project_dir = project_dir

                if proj_info:
                    descrip = proj_info.get('description', {})
                    if descrip.get('library'):
                        target = '{0} library'.format(project_name)
                        self.targets.append(target)
                        self.args[target] = (project_name, 'lib', '')

                    for exe in descrip.get('executables', []):
                        target = '{0} ({1} executable)'.format(exe['name'], project_name)
                        self.targets.append(target)
                        self.args[target] = (project_name, 'exe', exe['name'])

                    for test in descrip.get('tests', []):
                        target = '{0} ({1} test)'.format(test['name'], project_name)
                        self.targets.append(target)
                        self.args[target] = (project_name, 'test', test['name'])

                len_targets = len(self.targets)
                if len_targets == 1:
                    self.on_done(0)
                elif len_targets > 1:
                    self.window.show_quick_panel(self.targets, self.on_done)
                else:
                    Common.sublime_status_message('No target found for REPL.')
            else:
                Common.sublime_status_message("Not in project")
        else:
            Common.sublime_status_message("No file active")

    def on_done(self, idx):
        if idx >= 0:
            view = self.window.active_view()

            project_builder = Settings.get_project_setting(view, 'haskell_build_tool', Settings.PLUGIN.haskell_build_tool)
            (pkg, comp, target) = self.args[self.targets[idx]]
            if project_builder in ['cabal', 'cabal-new-build']:
                repl_target = ':'.join([comp, target])
            elif project_builder in ['stack']:
                repl_target = ':'.join([pkg, comp, target] if comp != 'lib' else [pkg, comp])

            external_id = 'Haskell_{0}_repl_{1}'.format(project_builder, repl_target).translate(self.FILE_NAME_TRANS)
            repl_views = list(sublimerepl.manager.find_repl(external_id))
            if not repl_views:
                repl_cmd_args = []
                ghci_opts = Settings.get_project_setting(view, 'ghci_opts', Settings.PLUGIN.ghci_opts)
                if ghci_opts:
                    repl_cmd_args += ['--ghci-options=' + ghci_opts]
                else:
                    ghc_opts = Settings.get_project_setting(view, 'ghc_opts', Settings.PLUGIN.ghc_opts)
                    if ghc_opts:
                        repl_cmd_args += ['--ghc-options' + ghc_opts]

                repl_cmd_args.append(repl_target)
                repl_cmd_args = repl_wrapper_cmd(project_builder, repl_cmd_args)
                Logging.log('repl cmd args: {0}'.format(repl_cmd_args), Logging.LOG_DEBUG)

                self.window.run_command("repl_open", repl_args(cmd=repl_cmd_args,
                                                               cwd=self.project_dir,
                                                               loaded=self.project_dir,
                                                               external_id=external_id))
            else:
                # Already have a REPL for this project, switch focus to it.
                for repl in repl_views:
                    win = repl.view.window()
                    if win:
                        win.focus_view(repl.view)


    def is_enabled(self):
        return HAS_SUBLIME_REPL and Common.is_enabled_haskell_command(None, True)


class SublimeHaskellReplLoadCommand(CommandWin.SublimeHaskellWindowCommand):
    def run(self, **_kwargs):
        view = self.window.active_view()
        if not view:
            Common.sublime_status_message("No file active")
        else:
            project_dir = Common.locate_cabal_project_from_view(view)[0]
            if not project_dir:
                self.window.run_command("sublime_haskell_repl_ghci_current_file", {})
            else:
                self.window.run_command("sublime_haskell_repl_cabal", {})

    def is_enabled(self):
        return HAS_SUBLIME_REPL and CommandWin.SublimeHaskellWindowCommand.is_enabled(self)
