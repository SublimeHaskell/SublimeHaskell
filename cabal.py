# -*- coding: UTF-8 -*-

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.hsdev.agent as hsdev


class SublimeHaskellCabalList(Common.SublimeHaskellWindowCommand):
    def __init__(self, view):
        super().__init__(view)
        self.packages = []

    def run(self):
        self.window.show_input_panel("Cabal list", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, inp):
        self.packages = hsdev.client.cabal_list(input)
        if not self.packages:
            Common.show_status_message("Package {0} not found".format(inp))
        else:
            self.window.show_quick_panel([([p.name] + ([p.synopsis] if p.synopsis else [])) for p in self.packages],
                                         self.on_select)

    def on_change(self, _):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return
        package = self.packages[idx]

        Common.output_panel(self.window, package.detailed(), 'sublime_haskell_cabal_package')
