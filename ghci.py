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
    dataRegex = '(?P<what>(newtype|type|data))\s+((?P<ctx>(.*))=>\s+)?{0}\s+(?P<args>(\w+\s+)*)='.format(name)
    classRegex = '(?P<what>class)\s+((?P<ctx>(.*))=>\s+)?{0}\s+(?P<args>(\w+\s+)*)(.*)where$'.format(name)

    if name[0].isupper():
        # data, class, type or newtype
        matched = re.search(dataRegex, contents, re.MULTILINE) or re.search(classRegex, contents, re.MULTILINE)
        if matched:
            what = matched.group('what')
            args = matched.group('args').strip().split(' ') if matched.group('args') else []
            ctx = matched.group('ctx')

            if what == 'class':
                return symbols.Class(name, ctx, args)
            elif what == 'data':
                return symbols.Data(name, ctx, args)
            elif what == 'type':
                return symbols.Type(name, ctx, args)
            elif what == 'newtype':
                return symbols.Newtype(name, ctx, args)
            else:
                raise RuntimeError('Unknown type of symbol: {0}'.format(what))

    else:
        # function
        matched = re.search(functionRegex, contents, re.MULTILINE)
        if matched:
            return symbols.Function(name, matched.group('type'))

    return None

def ghci_info(module, name):
    """
    Returns info for name as dictionary with fields:
    'module' -> module
    'name' -> name
    'what' -> one of 'function', 'class', 'data', 'type' or 'newtype'
    'type' -> type of function (e.g. 'Monad m => (a -> m b) -> [a] -> m [b]' for mapM)
    'args' -> args of 'class' or 'data', list of names (e.g. ['k', 'a'] for Data.Map.Map)
    'ctx' -> context of 'class' or 'data'
    """
    ghci_cmd = [
        ":m + " + module,
        ":i " + module + "." + name,
        ":q"]
    ghc_opts = get_setting_async('ghc_opts')

    (exit_code, stdout, stderr) = call_and_wait_with_input(ghci_append_package_db(['ghci'] + ghc_opts), "\n".join(ghci_cmd))
    stdout = crlf2lf(stdout)
    if exit_code == 0:
        return parse_info(name, stdout)

    return None

def ghci_package_db():
    dev = get_setting_async('use_cabal_dev')
    box = get_setting_async('cabal_dev_sandbox')
    if dev and box:
        package_conf = (filter(lambda x: re.match('packages-(.*)\.conf', x), os.listdir(box)) + [None])[0]
        if package_conf:
            return os.path.join(box, package_conf)
    return None

def ghci_append_package_db(cmd):
    package_conf = ghci_package_db()
    if package_conf:
        cmd.extend(['-package-db', package_conf])
    return cmd
