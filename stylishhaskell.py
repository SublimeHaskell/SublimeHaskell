import sublime
import sublime_plugin

from sublime_haskell_common import log, is_enabled_haskell_command, call_and_wait_with_input

class SublimeHaskellStylish(sublime_plugin.TextCommand):
    def run(self, edit):
        regions = []
        for region in self.view.sel():
            regions.append(sublime.Region(region.a, region.b))
            if region.empty():
                selection = sublime.Region(0, self.view.size())
            else:
                selection = region
            sel_str = self.view.substr(selection)
            exit_code, out, err = call_and_wait_with_input(['stylish-haskell'], sel_str)
            if exit_code == 0 and out != sel_str:
                self.view.replace(edit, selection, out)

        self.view.sel().clear()
        for region in regions:
            self.view.sel().add(region)

    def is_enabled(self):
        return is_enabled_haskell_command(False)
