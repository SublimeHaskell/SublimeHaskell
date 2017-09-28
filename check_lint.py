# -*- coding: UTF-8 -*-

import os

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.hsdev.result_parse as HsResultParse
import SublimeHaskell.internals.backend_mgr as BackendMgr
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.utils as Utils


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

    def run(self, edit):
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
        output_messages = [ParseOutput.OutputMessage(m['source']['file'],
                                                     HsResultParse.parse_region(m['region']).to_zero_based(),
                                                     m['level'].capitalize() + ': ' + m['note']['message'].replace('\n', '\n  '),
                                                     m['level']) for m in self.msgs]

        self.corrections = corrections or []
        for corr in self.corrections:
            corr.message_region.to_zero_based()
        self.corrections_dict = dict(((os.path.normpath(c.file), c.message_region.start.line, c.message_region.start.column), c)
                                     for c in self.corrections)

        for omsg in output_messages:
            okey = (os.path.normpath(omsg.filename), omsg.region.start.line, omsg.region.start.column)
            if okey in self.corrections_dict:
                omsg.correction = self.corrections_dict[okey]

        ParseOutput.set_global_error_messages(output_messages)
        output_text = ParseOutput.format_output_messages(output_messages)
        if Settings.PLUGIN.show_error_window:
            cabal_proj_dir = Common.get_cabal_project_dir_of_file(self.filename) or os.path.dirname(self.filename)
            panel_display = not self.fly_mode and len(output_messages) > 0
            sublime.set_timeout(lambda: ParseOutput.write_output(self.view, output_text, cabal_proj_dir, panel_display), 0)
        sublime.set_timeout(lambda: ParseOutput.mark_messages_in_views(output_messages), 0)

    def is_enabled(self):
        return Common.is_haskell_source(None) and super().is_enabled()


class SublimeHaskellCheck(SublimeHaskellHsDevChain):
    def __init__(self, view):
        super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellCheck', self.run_chain, [hsdev_check()], 'Checking', fly_mode=kwargs.get('fly', False))


class SublimeHaskellLint(SublimeHaskellHsDevChain):
    def __init__(self, view):
        super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellLint', self.run_chain, [hsdev_lint()], 'Linting', fly_mode=kwargs.get('fly', False))


class SublimeHaskellCheckAndLint(SublimeHaskellHsDevChain):
    def __init__(self, view):
        super().__init__(view)

    def run(self, _edit, **kwargs):
        Utils.run_async('SublimeHaskellCheckAndLint', self.run_chain, [hsdev_check(), hsdev_lint()], 'Checking and Linting',
                        fly_mode=kwargs.get('fly', False))
