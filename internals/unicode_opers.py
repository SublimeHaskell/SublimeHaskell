
import re

import SublimeHaskell.internals.settings as Settings

UNICODE_OPERS = {
    re.compile(r'(\s)->(\s)'): '\\1\u2192\\2',
    re.compile(r'(\s)=>(\s)'): '\\1\u21d2\\2',
    re.compile(r'(\s)::(\s)'): '\\1\u2237\\2',
    re.compile(r'(\s)-&gt;(\s)'): '\\1\u2192\\2',
    re.compile(r'(\s)=&gt;(\s)'): '\\1\u21d2\\2',
    re.compile(r'(\s)::(\s)'): '\\1\u2237\\2'
    }

def use_unicode_operators(src, force=False):
    '''Replace some standard haskell operators with Unicode equivalents. Prettifies the output presentation.

    Note: Replacements are made only if whitespace surrounds the operator on both sides, e.g. ' :: ' or ' -> '. If you're one
    of those people for whom economy of space is a premium and like unreadable code, you'll be disappointed.
    '''
    if force or Settings.PLUGIN.unicode_symbol_info:
        repl = src[:]

        for oper, unicode_oper in UNICODE_OPERS.items():
            repl = re.sub(oper, unicode_oper, repl)
        return repl

    return src
