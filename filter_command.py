
import sublime

if int(sublime.version()) < 3000:
    import sublime_haskell_common as Common
    import internals.proc_helper as ProcHelper
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.proc_helper as ProcHelper

class SublimeHaskellFilterCommand(Common.SublimeHaskellTextCommand):
    """Utility class to run filter-like commands, for example, 'stylish-haskell' and 'hindent'. Error/diagnostic
    output is sent to the Haskell Run Output window."""
    OUTPUT_PANEL_NAME = 'haskell_run_output'

    def __init__(self, view):
        super(SublimeHaskellFilterCommand, self).__init__(view)
        self.indenter = None

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

                with ProcHelper.ProcHelper(self.indenter, sel_str) as p:
                    if p.process is not None:
                        exit_code, out, err = p.wait()
                        # stylish-haskell does not have a non-zero exit code if it errors out! (Surprise!)
                        # Not sure about hindent, but this seems like a safe enough test.
                        if err is None or len(err) == 0:
                            self.view.replace(edit, selection, out)
                        else:
                            stderr_out = '\n'.join(["{0} failed, stderr contents:".format(' '.join(self.indenter)), "-" * 40, ""]) + err
                            self.report_error(stderr_out)
                    else:
                        self.report_error(p.process_err)


            # Questionable whether regions should be re-activated: stylish-haskell usually adds whitespace, which makes
            # the selection nonsensical.
            self.view.sel().clear()
            for region in regions:
                self.view.sel().add(region)

        except OSError as e:
            self.report_error('Exception executing {0}: {1}'.format(' '.join(self.indenter), e))

    def report_error(self, errmsg):
        window = self.view.window()
        output_view = window.get_output_panel(SublimeHaskellFilterCommand.OUTPUT_PANEL_NAME)
        output_view.run_command('sublime_haskell_output_text', {'text': errmsg, 'clear': 'yes'})
        output_view.sel().clear()
        output_view.sel().add(sublime.Region(0, 0))
        window.run_command('show_panel', {'panel': ('output.' + SublimeHaskellFilterCommand.OUTPUT_PANEL_NAME)})
