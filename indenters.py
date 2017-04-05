# -*- coding: UTF-8 -*-

import traceback

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.proc_helper as ProcHelper

class SublimeHaskellFilterCommand(Common.SublimeHaskellTextCommand):
    """Utility class to run filter-like commands, for example, 'stylish-haskell' and 'hindent'. Error/diagnostic
    output is sent to the Haskell Run Output window."""
    OUTPUT_PANEL_NAME = 'haskell_run_output'

    def __init__(self, view, indenter=None):
        super().__init__(view)
        self.indenter = indenter

    def run(self, edit):
        try:
            window = self.view.window()
            window.run_command('hide_panel', {'panel': 'output.' + SublimeHaskellFilterCommand.OUTPUT_PANEL_NAME})
            regions = []
            for region in self.view.sel():
                regions.append(sublime.Region(region.a, region.b))
                if region.empty():
                    selection = sublime.Region(0, self.view.size())
                else:
                    selection = region

                # Newline conversion seems dubious here, but... leave it alone for the time being.
                sel_str = self.view.substr(selection).replace('\r\n', '\n')

                with ProcHelper.ProcHelper(self.indenter) as proc:
                    if proc.process is not None:
                        _, out, err = proc.wait(sel_str)
                        # stylish-haskell does not have a non-zero exit code if it errors out! (Surprise!)
                        # Not sure about hindent, but this seems like a safe enough test.
                        if err is None or len(err) == 0:
                            self.view.replace(edit, selection, out)
                        else:
                            indent_err = ' '.join(self.indenter)
                            stderr_out = '\n'.join(["{0} failed, stderr contents:".format(indent_err), "-" * 40, ""]) + err
                            self.report_error(stderr_out)
                    else:
                        self.report_error(proc.process_err)


            # Questionable whether regions should be re-activated: stylish-haskell usually adds whitespace, which makes
            # the selection nonsensical.
            self.view.sel().clear()
            for region in regions:
                self.view.sel().add(region)

        except OSError:
            self.report_error('Exception executing {0}'.format(' '.join(self.indenter)))
            traceback.print_exc()

    def report_error(self, errmsg):
        window = self.view.window()
        output_view = window.get_output_panel(SublimeHaskellFilterCommand.OUTPUT_PANEL_NAME)
        output_view.run_command('sublime_haskell_output_text', {'text': errmsg, 'clear': 'yes'})
        output_view.sel().clear()
        output_view.sel().add(sublime.Region(0, 0))
        window.run_command('show_panel', {'panel': ('output.' + SublimeHaskellFilterCommand.OUTPUT_PANEL_NAME)})

class SublimeHaskellStylish(SublimeHaskellFilterCommand):
    def __init__(self, view):
        super().__init__(view, indenter=["stylish-haskell"])

class SublimeHaskellHindent(SublimeHaskellFilterCommand):
    def __init__(self, view):
        super().__init__(view, indenter=["hindent"])
