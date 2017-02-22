# -*- coding: UTF-8 -*-

# Sublime's built-in Haskell lexer (Packages/Haskell/Haskell.tmLanguage)
# is slightly broken.
#
# This sets the syntax of all Haskell files to our
# SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.tmLanguage instead.
#
# Forked from https://gist.github.com/2940866.
import sublime_plugin
import os

if int(sublime.version()) < 3000:
    from sublime_haskell_common import get_settings_async
else:
    from SublimeHaskell.sublime_haskell_common import get_settings_async


class DetectFileTypeCommand(sublime_plugin.EventListener):

    def on_load(self, view):
        if get_settings_async('use_improved_syntax', True):
            filename = view.file_name()
            if not filename:  # buffer has never been saved
                return

            name = os.path.basename(filename.lower())
            if name.endswith(".hs") or name.endswith(".hsc"):
                set_our_syntax(view, filename)
        # TODO Do we also have to fix Literate Haskell?


def set_our_syntax(view, filename):
    view.settings().set('syntax', 'Packages/SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.tmLanguage')
    print("Switched syntax to SublimeHaskell's fixed Haskell syntax: " + filename)
