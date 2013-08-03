import re
import os
import sublime
import sublime_plugin

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import symbols
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.symbols as symbols

def parse_info(name, contents):
    """
    Parses result of :i <name> command of ghci and returns derived symbols.Declaration
    """
    functionRegex = '{0}\s+::\s+(?P<type>.*?)(\s+--(.*))?$'.format(name)
    dataRegex = '(?P<what>(newtype|type|data))\s+((?P<ctx>(.*))=>\s+)?(?P<name>\S+)\s+(?P<args>(\w+\s+)*)=(\s*(?P<def>.*)\s+-- Defined)?'
    classRegex = '(?P<what>class)\s+((?P<ctx>(.*))=>\s+)?(?P<name>\S+)\s+(?P<args>(\w+\s+)*)(.*)where$'

    if name[0].isupper():
        # data, class, type or newtype
        matched = re.search(dataRegex, contents, re.DOTALL) or re.search(classRegex, contents, re.DOTALL)
        if matched:
            what = matched.group('what')
            args = matched.group('args').strip().split(' ') if matched.group('args') else []
            ctx = matched.group('ctx')
            definition = matched.group('def')
            if definition:
                definition.strip()

            if what == 'class':
                return symbols.Class(name, ctx, args)
            elif what == 'data':
                return symbols.Data(name, ctx, args, definition)
            elif what == 'type':
                return symbols.Type(name, ctx, args, definition)
            elif what == 'newtype':
                return symbols.Newtype(name, ctx, args, definition)
            else:
                raise RuntimeError('Unknown type of symbol: {0}'.format(what))

    else:
        # function
        matched = re.search(functionRegex, contents, re.DOTALL)
        if matched:
            return symbols.Function(name, matched.group('type'))

    return None

def ghci_info(module, name, cabal = None):
    """
    Returns info for name as symbol
    """
    ghci_cmd = [
        ":m + " + module,
        ":i " + module + "." + name,
        ":q"]
    ghc_opts = get_setting_async('ghc_opts')

    (exit_code, stdout, stderr) = call_and_wait_with_input(ghci_append_package_db(['ghci'] + ghc_opts, cabal = cabal), "\n".join(ghci_cmd))
    stdout = crlf2lf(stdout)
    if exit_code == 0:
        return parse_info(name, stdout)

    return None
