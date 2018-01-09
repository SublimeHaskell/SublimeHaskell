# -*- coding: UTF-8 -*-

import traceback

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings

FILTER_OUTPUT_PANEL_NAME = 'haskell_run_output'

def do_prettify(view, edit, indenter, indenter_options):
    try:
        window = view.window()
        window.run_command('hide_panel', {'panel': 'output.' + FILTER_OUTPUT_PANEL_NAME})
        regions = []
        for region in view.sel():
            regions.append(sublime.Region(region.a, region.b))
            selection = region if not region.empty() else sublime.Region(0, view.size())

            # Newline conversion seems dubious here, but... leave it alone for the time being.
            sel_str = view.substr(selection).replace('\r\n', '\n')

            with ProcHelper.ProcHelper(indenter + indenter_options) as proc:
                if proc.process is not None:
                    _, out, err = proc.wait(sel_str)
                    # stylish-haskell does not have a non-zero exit code if it errors out! (Surprise!)
                    # Not sure about hindent, but this seems like a safe enough test.
                    #
                    # Also test if the contents actually changed so break the save-indent-save-indent-... loop if
                    # the user enabled prettify_on_save.
                    if not err:
                        if out not in [selection, sel_str]:
                            view.replace(edit, selection, out)
                    else:
                        indent_err = ' '.join(indenter)
                        stderr_out = '\n'.join(["{0} failed, stderr contents:".format(indent_err), "-" * 40, ""]) + err
                        report_error(view, stderr_out)
                else:
                    report_error(view, proc.process_err)


        view.sel().clear()
        # Questionable whether regions should be re-activated: stylish-haskell usually adds whitespace, which makes
        # the selection nonsensical.
        #
        # However, there are other plugins that get fired after SublimeHaskell that don't like it when you kill all of the
        # selection regions from underneath their feet.
        for region in regions:
            view.sel().add(region)

    except OSError:
        report_error(view, 'Exception executing {0}'.format(' '.join(indenter)))
        traceback.print_exc()

def report_error(view, errmsg):
    window = view.window()
    output_view = window.get_output_panel(FILTER_OUTPUT_PANEL_NAME)
    output_view.run_command('sublime_haskell_output_text', {'text': errmsg, 'clear': 'yes'})
    window.run_command('show_panel', {'panel': ('output.' + FILTER_OUTPUT_PANEL_NAME)})
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0, 0))

class SublimeHaskellStylish(CommandWin.SublimeHaskellTextCommand):
    def run(self, edit, **_kwargs):
        stylish_options = Settings.get_project_setting(self.view, 'stylish_options', Settings.PLUGIN.stylish_options)
        do_prettify(self.view, edit, ['stylish-haskell'], stylish_options)

class SublimeHaskellHindent(CommandWin.SublimeHaskellTextCommand):
    def run(self, edit, **_kwargs):
        hindent_options = Settings.get_project_setting(self.view, 'hindent_options', Settings.PLUGIN.hindent_options)
        do_prettify(self.view, edit, ['hindent'], hindent_options)
