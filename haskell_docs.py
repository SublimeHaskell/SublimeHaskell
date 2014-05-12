import re
import os
import sublime
import sublime_plugin

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
else:
    from SublimeHaskell.sublime_haskell_common import *

def haskell_docs(module, name):
    """
    Returns info for name as multiline string
    """
    try:
        (exit_code, stdout, stderr) = call_and_wait(["haskell-docs", module, name])
        stdout = crlf2lf(stdout)
        if exit_code == 0:
            ambigousRe = '^Ambiguous module, belongs to more than one package: (.*)$'
            continueRe = '^Continuing anyway... $'
            cantFindRe = '^Couldn\'t find name ``{0}\'\' in Haddock interface: (.*)$'.format(name)
            packageRe = '^Package: (.*)$'
            ignoreRe = '({0})|({1})|({2})|({3})'.format(ambigousRe, continueRe, cantFindRe, packageRe)

            # Remove debug messages
            result = list(filter(lambda l: not re.match(ignoreRe, l), stdout.splitlines()))
            return '\n'.join(result)
    except OSError as e:
        if e.errno == errno.ENOENT:
            log("haskell-docs not found, no docs available, try 'cabal install haskell-docs'", log_error)
    return None
