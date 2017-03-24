# -*- coding: UTF-8 -*-

import sublime

import SublimeHaskell.filter_command as Filtercommand

class SublimeHaskellStylish(Filtercommand.SublimeHaskellFilterCommand):
    def __init__(self, view):
        super().__init__(view)
        self.indenter = ["stylish-haskell"]

class SublimeHaskellHindent(Filtercommand.SublimeHaskellFilterCommand):
    def __init__(self, view):
        super().__init__(view)
        self.indenter = ["hindent"]
