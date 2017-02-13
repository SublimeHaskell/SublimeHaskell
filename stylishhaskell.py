# -*- coding: UTF-8 -*-

import errno
import sublime

if int(sublime.version()) < 3000:
    from sublime_haskell_common import is_enabled_haskell_command, crlf2lf, ProcHelper, SublimeHaskellTextCommand
else:
    from SublimeHaskell.sublime_haskell_common import is_enabled_haskell_command, crlf2lf, ProcHelper, SublimeHaskellTextCommand


class SublimeHaskellStylish(SublimeHaskellTextCommand):
    def run(self, edit):
        try:
            regions = []
            for region in self.view.sel():
                regions.append(sublime.Region(region.a, region.b))
                if region.empty():
                    selection = sublime.Region(0, self.view.size())
                else:
                    selection = region
                sel_str = self.view.substr(selection).replace('\r\n', '\n')
                with ProcHelper(['stylish-haskell'], sel_str) as p:
                    if p.process is not None:
                        exit_code, out, err = p.wait()
                        out_str = crlf2lf(out)
                        if exit_code == 0 and out_str != sel_str:
                            self.view.replace(edit, selection, out_str)

            self.view.sel().clear()
            for region in regions:
                self.view.sel().add(region)

        except OSError as e:
            if e.errno == errno.ENOENT:
                sublime.error_message("SublimeHaskell: stylish-haskell was not found!")

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)