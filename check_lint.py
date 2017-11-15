# -*- coding: UTF-8 -*-

import os

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.hsdev.result_parse as HsResultParse
import SublimeHaskell.internals.backend_mgr as BackendMgr
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.symbols as Symbols


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
    def __init__(self, view):
        super().__init__(view)
        self.msgs = []
        self.corrections = []
        self.corrections_dict = {}
        self.fly_mode = False
        self.filename = None
        self.contents = {}
        self.status_msg = None

    def run(self, edit, **_args):
        print('SublimeHaskellHsDevChain.run??')
        raise NotImplementedError("SublimeHaskellDevChain.run needs an implementation.")

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
                    self.status_msg = Common.status_message_process(msg + ': ' + self.filename, priority=2)
                    self.status_msg.start()
                    self.go_chain(cmds)
                else:
                    sublime.error_message('Empty command chain (check_lint.run_chain)')

    def go_chain(self, cmds):
        if cmds:
            cmd, tail_cmds = cmds[0], cmds[1:]
            agent_func, modify_args, kwargs = cmd

            def go_chain_resp(msgs):
                self.msgs.extend(msgs)
                self.go_chain(tail_cmds)

            def go_chain_err(_err, _details):
                self.status_msg.fail()
                self.go_chain([])

            agent_func(modify_args(self.filename), contents=self.contents, wait_complete=False,
                       on_response=go_chain_resp, on_error=go_chain_err, **kwargs)
        else:
            self.status_msg.stop()
            BackendMgr.active_backend().autofix_show(self.msgs, on_response=self.on_autofix, wait_complete=False)

    def on_autofix(self, corrections):
        # Oh, this looks pretty ugly. But, list comprehensions are supposedly faster than loops. And since this is
        # is supporting Haskell, why not use the functional approach? :-)
        output_messages = [ParseOutput.OutputMessage(msg.get('source', {}).get('file', '<no file/command line/OPTIONS_GHC>'),
                                                     HsResultParse.parse_region(msg.get('region')).to_zero_based() \
                                                       if msg.get('region') is not None else Symbols.Region(0),
                                                     msg.get('level', 'uncategorized').capitalize() + ': ' + \
                                                       msg.get('note', {}).get('message', '').replace('\n', '\n  '),
                                                     msg.get('level', 'uncategorized'))
                           for msg in self.msgs]

        # Hack alert: Region and Position.to_zero_based() return the original object (self) after updating it. 'and' returns the
        # right hand side, which is never None or a false value.
        self.corrections_dict = dict(((os.path.normpath(c.file), c.message_region.start.line, c.message_region.start.column), c)
                                     for c in [corr.message_region.to_zero_based() and corr for corr in corrections or []])

        for omsg in output_messages:
            okey = (os.path.normpath(omsg.filename), omsg.region.start.line, omsg.region.start.column)
            if okey in self.corrections_dict:
                omsg.correction = self.corrections_dict[okey]

        ParseOutput.set_global_error_messages(output_messages)
        output_text = ParseOutput.format_output_messages(output_messages)
        if Settings.PLUGIN.show_error_window:
            cabal_proj_dir = Common.get_cabal_project_dir_of_file(self.filename) or os.path.dirname(self.filename)
            panel_display = not self.fly_mode and output_messages
            sublime.set_timeout(lambda: ParseOutput.write_output(self.view, output_text, cabal_proj_dir, panel_display), 0)
        sublime.set_timeout(lambda: ParseOutput.mark_messages_in_views(output_messages), 0)

    def is_enabled(self):
        return Common.is_haskell_source(None) and super().is_enabled()


class SublimeHaskellCheck(SublimeHaskellHsDevChain):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellCheck', self.run_chain, [hsdev_check()], 'Checking', fly_mode=kwargs.get('fly', False))


def exec_check_process(view):
    '''Utility function to unconditionally execute `SublimeHaskellCheck.run()` without worrying about the command's status.
    '''
    return SublimeHaskellCheck(view).run(None)


class SublimeHaskellLint(SublimeHaskellHsDevChain):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellLint', self.run_chain, [hsdev_lint()], 'Linting', fly_mode=kwargs.get('fly', False))


def exec_lint_process(view):
    '''Utility function to unconditionally execute `SublimeHaskellLint.run()` without worrying about the command's status.
    '''
    return SublimeHaskellLint(view).run(None)


class SublimeHaskellCheckAndLint(SublimeHaskellHsDevChain):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellCheckAndLint', self.run_chain, [hsdev_check(), hsdev_lint()], 'Checking and Linting',
                        fly_mode=kwargs.get('fly', False))


def exec_check_and_lint_process(view):
    '''Utility function to unconditionally execute 'SublimeHaskellCHeckAndLint.run()' without worrying
    about the command's status.
    '''
    return SublimeHaskellCheckAndLint(view).run(None)
