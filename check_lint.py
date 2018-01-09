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
    def __init__(self, view, on_done=None):
        super().__init__()
        self.view = view
        self.contents = {}
        self.corrections = []
        self.corrections_dict = {}
        self.filename = None
        self.fly_mode = False
        self.msgs = []
        self.status_msg = None
        self.on_done_callback = on_done


    def run_chain(self, cmds, msg, fly_mode=False):
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
                self.go_chain(cmds)
            else:
                sublime.error_message('Empty command chain (check_lint.run_chain)')
        else:
            print('run_chain: no file name? {0}'.format(self.filename))


    def go_chain(self, cmds):
        if not cmds:
            self.on_done()
        else:
            def on_error(_exc, _details):
                self.on_error()

            def on_response(resp):
                self.msgs.extend(resp)
                self.go_chain(cmds)

            agent_func, modify_args, kwargs = cmds.pop(0)
            agent_func(
                modify_args(self.filename),
                contents=self.contents,
                wait_complete=False,
                on_error=on_error,
                on_response=on_response,
                **kwargs
            )

    def on_error(self):
        self.status_msg.result_fail()
        if self.on_done_callback:
            self.on_done_callback(False)

    def on_done(self):
        self.status_msg.result_ok()
        if self.on_done_callback:
            self.on_done_callback(True)
        sublime.set_timeout(self.show_autofixes, 0)

    def show_autofixes(self):
        corrections = BackendMgr.active_backend().autofix_show(self.msgs, True)
        ParseOutput.MARKER_MANAGER.mark_response(self.view, self.msgs, corrections, self.fly_mode)


def exec_check(view, fly_mode=False, on_done=None):
    chain_runner = ChainRunner(view, on_done=on_done)
    chain_runner.run_chain([hsdev_check()], 'Checking', fly_mode=fly_mode)


def exec_lint(view, fly_mode=False, on_done=None):
    '''Utility function to unconditionally execute `SublimeHaskellLint.run()` without worrying about the command's status.
    '''
    chain_runner = ChainRunner(view, on_done=on_done)
    chain_runner.run_chain([hsdev_lint()], 'Linting', fly_mode=fly_mode)


def exec_check_and_lint(view, fly_mode=False, on_done=None):
    '''Utility function to unconditionally execute 'SublimeHaskellCHeckAndLint.run()' without worrying
    about the command's status.
    '''
    chain_runner = ChainRunner(view, on_done=on_done)
    chain_runner.run_chain([hsdev_check(), hsdev_lint()], 'Checking and Linting', fly_mode=fly_mode)


class SublimeHaskellCheck(CommandWin.HaskellSourceBackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_check(self.view, fly_mode=kwargs.get('fly', False))


class SublimeHaskellLint(CommandWin.BackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_lint(self.view, fly_mode=kwargs.get('fly', False))


class SublimeHaskellCheckAndLint(CommandWin.BackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_check_and_lint(self.view, fly_mode=kwargs.get('fly', False))
