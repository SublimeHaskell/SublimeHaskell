# -*- coding: UTF-8 -*-

import errno
import sublime

if int(sublime.version()) < 3000:
    from sublime_haskell_common import is_enabled_haskell_command, ProcHelper, SublimeHaskellTextCommand
else:
    from SublimeHaskell.sublime_haskell_common import is_enabled_haskell_command, ProcHelper, SublimeHaskellTextCommand

OUTPUT_PANEL_NAME = 'sublime_haskell_output_panel'

class SublimeHaskellStylish(SublimeHaskellTextCommand):
    def run(self, edit):
        try:
            window = self.view.window()
            window.run_command('hide_panel', {'panel': 'output' + OUTPUT_PANEL_NAME})
            regions = []
            for region in self.view.sel():
                regions.append(sublime.Region(region.a, region.b))
                if region.empty():
                    selection = sublime.Region(0, self.view.size() - 1)
                else:
                    selection = region
                sel_str = self.view.substr(selection).replace('\r\n', '\n')
                with ProcHelper(['stylish-haskell'], sel_str) as p:
                    if p.process is not None:
                        exit_code, out, err = p.wait()
                        # stylish-haskell does not have a non-zero exit code if it errors out! (Surprise!)
                        if err is None or len(err) == 0:
                            self.view.replace(edit, selection, out)
                        else:
                            stderr_out = '\n'.join(["stylish-haskell failed, stderr contents:", "-" * 40, ""]) + err
                            output_view = window.get_output_panel(OUTPUT_PANEL_NAME)
                            output_view.run_command('sublime_haskell_output_text', {'text': stderr_out, 'clear': 'yes'})
                            output_view.sel().clear()
                            output_view.sel().add(sublime.Region(0, 0))
                            window.run_command('show_panel', {'panel': ('output.' + OUTPUT_PANEL_NAME)})

            # Questionable whether regions should be re-activated: stylish-haskell usually adds whitespace, which makes
            # the selection nonsensical.
            self.view.sel().clear()
            for region in regions:
                self.view.sel().add(region)

        except OSError as e:
            if e.errno == errno.ENOENT:
                sublime.error_message("SublimeHaskell: stylish-haskell was not found!")
