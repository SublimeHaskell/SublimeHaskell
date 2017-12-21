# -*- coding: UTF-8 -*-

import os
import threading

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.internals.backend_mgr as BackendMgr
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
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


class SublimeHaskellHsDevChain(CommandWin.BackendTextCommand):
    RUN_CHAIN_FLAG = threading.Event()
    '''Event flag used to signal that a command chain has run to completion.
    '''

    def __init__(self, view):
        super().__init__(view)
        self.contents = {}
        self.corrections = []
        self.corrections_dict = {}
        self.filename = None
        self.fly_mode = False
        self.msgs = []
        self.status_msg = None


    def run(self, edit, **_args):
        print('SublimeHaskellHsDevChain.run??')
        raise NotImplementedError("SublimeHaskellDevChain.run needs an implementation.")


    def run_chain(self, cmds, msg, fly_mode=False):
        try:
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
                    self.status_msg = Common.status_message_process(msg + ': ' + self.filename)
                    self.status_msg.start()
                    self.go_chain(cmds)
                else:
                    sublime.error_message('Empty command chain (check_lint.run_chain)')
            else:
                print('run_chain: no file name? {0}'.format(self.filename))
        finally:
            self.RUN_CHAIN_FLAG.set()


    def go_chain(self, cmds):
        if cmds:
            cmd, tail_cmds = cmds[0], cmds[1:]
            agent_func, modify_args, kwargs = cmd

            def go_chain_resp(msgs):
                self.msgs.extend(msgs)
                self.go_chain(tail_cmds)

            def go_chain_err(_err, _details):
                self.status_msg.result_fail()
                self.go_chain([])

            agent_func(modify_args(self.filename), contents=self.contents, wait_complete=False,
                       on_response=go_chain_resp, on_error=go_chain_err, **kwargs)
        else:
            self.status_msg.result_ok()
            BackendMgr.active_backend().autofix_show(self.msgs, False, on_response=self.on_autofix)


    def on_autofix(self, corrections):
        ParseOutput.MARKER_MANAGER.mark_response(self.view, self.msgs, corrections, self.fly_mode)

    def is_enabled(self):
        return Common.view_is_haskell_source(None) and super().is_enabled()


    @staticmethod
    def reset_chain_flag():
        SublimeHaskellHsDevChain.RUN_CHAIN_FLAG.clear()


    @staticmethod
    def run_chain_flag():
        return SublimeHaskellHsDevChain.RUN_CHAIN_FLAG


class SublimeHaskellCheck(SublimeHaskellHsDevChain):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellCheck', self.run_chain, [hsdev_check()], 'Checking', fly_mode=kwargs.get('fly', False))


def exec_check_process(view, fly=False):
    return SublimeHaskellCheck(view).run(None, fly=fly)


class SublimeHaskellLint(SublimeHaskellHsDevChain):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellLint', self.run_chain, [hsdev_lint()], 'Linting', fly_mode=kwargs.get('fly', False))


def exec_lint_process(view, fly=False):
    '''Utility function to unconditionally execute `SublimeHaskellLint.run()` without worrying about the command's status.
    '''
    return SublimeHaskellLint(view).run(None, fly=fly)


class SublimeHaskellCheckAndLint(SublimeHaskellHsDevChain):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellCheckAndLint', self.run_chain, [hsdev_check(), hsdev_lint()], 'Checking and Linting',
                        fly_mode=kwargs.get('fly', False))

def exec_check_and_lint_process(view, fly=False):
    '''Utility function to unconditionally execute 'SublimeHaskellCHeckAndLint.run()' without worrying
    about the command's status.
    '''
    return SublimeHaskellCheckAndLint(view).run(None, fly=fly)
