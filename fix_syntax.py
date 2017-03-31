# -*- coding: UTF-8 -*-

# Sublime's built-in Haskell lexer (Packages/Haskell/Haskell.tmLanguage)
# is slightly broken.
#
# This sets the syntax of all Haskell files to our
# SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.tmLanguage instead.
#
# Forked from https://gist.github.com/2940866.

import os

import sublime_plugin

import SublimeHaskell.internals.settings as Settings


class DetectFileTypeCommand(sublime_plugin.EventListener):

    def on_load(self, view):
        if Settings.PLUGIN.use_improved_syntax:
            filename = view.file_name()
            if not filename:  # buffer has never been saved
                return

            name = os.path.basename(filename.lower())
            if name.endswith(".hs") or name.endswith(".hsc"):
                view.settings().set('syntax', 'Packages/SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.tmLanguage')
                # print("Switched syntax to SublimeHaskell's fixed Haskell syntax: " + filename)

        # TODO Do we also have to fix Literate Haskell?
