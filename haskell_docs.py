import re
import os
import sublime_plugin

from sublime_haskell_common import *

def haskell_docs(module, name):
    """
    Returns info for name as multiline string
    """
    (exit_code, stdout, stderr) = call_and_wait(["haskell-docs", module, name])
    stdout
    if exit_code == 0:
        ambigousRe = '^Ambiguous module, belongs to more than one package: (.*)$'
        continueRe = '^Continuing anyway... $'
        cantFindRe = '^Couldn\'t find name ``{0}\'\' in Haddock interface: {1}$'.format(name, module.replace('.', '\\.'))
        packageRe = '^Package: (.*)$'
        ignoreRe = '({0})|({1})|({2})|({3})'.format(ambigousRe, continueRe, cantFindRe, packageRe)

        # Remove debug messages
        result = filter(lambda l: not re.match(ignoreRe, l), stdout.splitlines())
        return '\n'.join(result)
    return None
