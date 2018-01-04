# -*- coding: UTF-8 -*-

import threading

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.internals.backend_mgr as BackendMgr
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.sublime_haskell_common as Common


def file_as_file_list(file):
    '''Turn the file name into a singleton list. Used in hsdev_check() and hsdev_lint()
    '''
    return [file]

def hsdev_check():
    return (BackendMgr.active_backend().check, file_as_file_list, {'ghc': Settings.PLUGIN.ghc_opts})


def hsdev_lint():
    return (BackendMgr.active_backend().lint, file_as_file_list, {})


def messages_as_hints(cmd):
    return (cmd[0], cmd[1], lambda ms: [dict(m, level='hint') for m in ms], cmd[3])


class ChainRunner(object):
    def __init__(self, view):
        super().__init__()
        self.view = view
        self.contents = {}
        self.corrections = []
        self.corrections_dict = {}
        self.filename = None
        self.fly_mode = False
        self.msgs = []
        self.status_msg = None


    def run_chain(self, cmds, msg, fly_mode=False):
        retval = False
        self.filename = self.view.file_name()
        if self.filename:
            self.msgs = []
            self.corrections = []
            self.fly_mode = fly_mode
            self.contents = {}
            if self.view.is_dirty():
                self.contents[self.filename] = self.view.substr(sublime.Region(0, self.view.size()))
            if not self.fly_mode:
                ParseOutput.hide_output(self.view)
            if cmds:
                ParseOutput.MARKER_MANAGER.clear_error_marks()

                self.status_msg = Common.status_message_process(msg + ': ' + self.filename)
                self.status_msg.start()
                _, retval = self.go_chain(cmds)
                if retval:
                    self.status_msg.result_ok()
                else:
                    self.status_msg.result_fail()
            else:
                sublime.error_message('Empty command chain (check_lint.run_chain)')
        else:
            print('run_chain: no file name? {0}'.format(self.filename))

        return retval


    def go_chain(self, cmds):
        retval = ([], True)

        while cmds and retval[1]:
            agent_func, modify_args, kwargs = cmds[0]
            retval = agent_func(modify_args(self.filename), contents=self.contents, wait_complete=True,
                                result_convert=self.chain_result, on_error=self.chain_error, **kwargs)
            resp, status = retval
            if status:
                self.msgs.extend(resp)
                cmds = cmds[1:]

        return retval

    def chain_error(self, _exc, _details):
        return ([], False)

    def chain_result(self, resp):
        return (resp, True)


    def show_autofixes(self):
        corrections = BackendMgr.active_backend().autofix_show(self.msgs, True)
        ParseOutput.MARKER_MANAGER.mark_response(self.view, self.msgs, corrections, self.fly_mode)


def exec_check(view, fly_mode=False):
    chain_runner = ChainRunner(view)
    successful = chain_runner.run_chain([hsdev_check()], 'Checking', fly_mode=fly_mode)
    chain_runner.show_autofixes()
    return successful


def exec_lint(view, fly_mode=False,):
    '''Utility function to unconditionally execute `SublimeHaskellLint.run()` without worrying about the command's status.
    '''
    chain_runner = ChainRunner(view)
    successful = chain_runner.run_chain([hsdev_lint()], 'Linting', fly_mode=fly_mode)
    chain_runner.show_autofixes()
    return successful


def exec_check_and_lint(view, fly_mode=False,):
    '''Utility function to unconditionally execute 'SublimeHaskellCHeckAndLint.run()' without worrying
    about the command's status.
    '''
    chain_runner = ChainRunner(view)
    successful = chain_runner.run_chain([hsdev_check(), hsdev_lint()], 'Checking and Linting', fly_mode=fly_mode)
    chain_runner.show_autofixes()
    return successful


class SublimeHaskellCheck(CommandWin.HaskellSourceBackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_check(self.view, fly_mode=kwargs.get('fly', False))


class SublimeHaskellLint(CommandWin.BackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_lint(self.view, fly_mode=kwargs.get('fly', False))


class SublimeHaskellCheckAndLint(CommandWin.BackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_check_and_lint(self.view, fly_mode=kwargs.get('fly', False))
