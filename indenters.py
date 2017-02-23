# -*- coding: UTF-8 -*-

import sublime

if int(sublime.version()) < 3000:
    import filter_command as FC
else:
    import SublimeHaskell.filter_command as FC

class SublimeHaskellStylish(FC.SublimeHaskellFilterCommand):
    def __init__(self, view):
        super(SublimeHaskellStylish, self).__init__(view)
        self.indenter = ["stylish-haskell"]

class SublimeHaskellHindent(FC.SublimeHaskellFilterCommand):
    def __init__(self, view):
        super(SublimeHaskellHindent, self).__init__(view)
        self.indenter = ["hindent"]
