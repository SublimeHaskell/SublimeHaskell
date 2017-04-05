
import html

import SublimeHaskell.internals.settings as Settings

UNICODE_OPERS = {
    '->': '\u2192',
    '=>': '\u21d2',
    '::': '\u2237'
    }

UNICODE_OPERS.update(dict((html.escape(oper), repl) for oper, repl in UNICODE_OPERS.items()))

def use_unicode_operators(src, force=False):
    """
    Set unicode symbols for some standard haskell operators
    """
    if force or Settings.PLUGIN.unicode_symbol_info:
        repl = src
        for oper, unicode_oper in UNICODE_OPERS.items():
            repl = repl.replace(oper, unicode_oper)
        return repl
    else:
        return src
