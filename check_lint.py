# -*- coding: UTF-8 -*-

import functools

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
    def __init__(self, view, caption, continue_success, error_handler):
        super().__init__()
        self.view = view
        self.contents = {}
        self.corrections = []
        self.corrections_dict = {}
        self.filename = view.file_name()
        self.caption = caption
        self.fly_mode = False
        self.msgs = []
        self.commands = []
        self.continue_success = continue_success
        self.error_handler = error_handler
        self.status_msg = None
        if view.is_dirty() and self.filename:
            self.contents[self.filename] = self.view.substr(sublime.Region(0, self.view.size()))


    def run_chain(self, cmds, fly_mode=False):
        ParseOutput.MARKER_MANAGER.clear_error_marks()
        if self.filename:
            self.fly_mode = fly_mode
            if not self.fly_mode:
                Common.hide_panel(self.view.window())
            if cmds:
                self.status_msg = Common.status_message_process(self.caption + ': ' + self.filename)
                self.status_msg.start()
                self.commands = cmds
                self.go_chain()
            else:
                sublime.error_message('Empty command chain (check_lint.run_chain)')
        else:
            print('run_chain: no file name? {0}'.format(self.filename))


    def go_chain(self):
        if self.commands:
            agent_func, modify_args, kwargs = self.commands[0]
            self.commands = self.commands[1:]
            agent_func(modify_args(self.filename), contents=self.contents, wait_complete=False,
                       on_response=self.next_in_chain, on_error=self.chain_error, **kwargs)
        else:
            self.status_msg.result_ok()
            BackendMgr.active_backend().autofix_show(self.msgs, wait_complete=False, on_response=self.process_corrections)


    def next_in_chain(self, resp):
        self.msgs.extend(resp)
        ## Leave room to expand the error message cases...
        if any([msg.get('level', '') in ['error'] for msg in resp]):
            self.status_msg.result_fail()
            ## Paranoia: Ensure that mark_response() executes in the UI thread
            BackendMgr.active_backend().autofix_show(self.msgs, wait_complete=False, on_response=self.process_corrections)
        else:
            self.go_chain()


    def chain_error(self, exc, details):
        # Fabricate an uncategorized error.
        self.msgs.append({'source': {'filename': details.get('module', {}).get('file', self.filename),
                                     'project': details.get('module', {}).get('project')},
                          'region': {'to': {'line': 1, 'column': 1},
                                     'from': {'line': 1, 'column': 1}},
                          'level': 'uncategorized',
                          'note': {'suggestion': None,
                                   'message': 'Backend error encountered during \'{0}\': {1}'.format(self.caption, exc)}})
        self.status_msg.result_fail()
        ## Paranoia: Ensure that mark_response() executes in the UI thread
        sublime.set_timeout(self.process_error, 0)


    def process_corrections(self, corrections):
        ## Ensure that show_autofixes runs in the UI thread.
        sublime.set_timeout(functools.partial(self.show_autofixes, corrections), 0)


    def show_autofixes(self, corrections):
        ParseOutput.MARKER_MANAGER.mark_response(self.view, self.msgs, corrections, self.fly_mode)
        if self.continue_success:
            self.continue_success(self.view)

    def process_error(self):
        ParseOutput.MARKER_MANAGER.mark_response(self.view, self.msgs, [], self.fly_mode)
        if self.error_handler:
            self.error_handler(self.view)


def exec_check(view, fly_mode=False, continue_success=None, error_handler=None):
    chain_runner = ChainRunner(view, 'Checking', continue_success, error_handler)
    chain_runner.run_chain([hsdev_check()], fly_mode=fly_mode)


def exec_lint(view, fly_mode=False, continue_success=None, error_handler=None):
    '''Utility function to unconditionally execute `SublimeHaskellLint.run()` without worrying about the command's status.
    '''
    chain_runner = ChainRunner(view, 'Linting', continue_success, error_handler)
    chain_runner.run_chain([hsdev_lint()], fly_mode=fly_mode)


def exec_check_and_lint(view, fly_mode=False, continue_success=None, error_handler=None):
    '''Utility function to unconditionally execute 'SublimeHaskellCHeckAndLint.run()' without worrying
    about the command's status.
    '''
    chain_runner = ChainRunner(view, 'Checking and Linting', continue_success, error_handler)
    chain_runner.run_chain([hsdev_check(), hsdev_lint()], fly_mode=fly_mode)


class SublimeHaskellCheck(CommandWin.HaskellSourceBackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_check(self.view, fly_mode=kwargs.get('fly', False))


class SublimeHaskellLint(CommandWin.BackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_lint(self.view, fly_mode=kwargs.get('fly', False))


class SublimeHaskellCheckAndLint(CommandWin.BackendTextCommand):
    def run(self, _edit, **kwargs):
        exec_check_and_lint(self.view, fly_mode=kwargs.get('fly', False))
