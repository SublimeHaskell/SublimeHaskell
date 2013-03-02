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
        functionRegex = '{0}\s+::\s+(?P<type>.*?)(\s+--(.*))?$'.format(name)
        dataRegex = '(?P<what>(newtype|type|data))\s+((?P<ctx>(.*))=>\s+)?{0}\s+(?P<args>(\w+\s+)*)='.format(name)
        classRegex = '(?P<what>class)\s+((?P<ctx>(.*))=>\s+)?{0}\s+(?P<args>(\w+\s+)*)(.*)where$'.format(name)

        result = {
            'module': module,
            'name': name }

        if name[0].isupper():
            # data or class
            matched = re.search(dataRegex, stdout, re.MULTILINE) or re.search(classRegex, stdout, re.MULTILINE)
            if matched:
                result['what'] = matched.group('what')
                result['args'] = matched.group('args').strip().split(' ') if matched.group('args') else []
                if matched.group('ctx'):
                    result['ctx'] = matched.group('ctx')
                return result
        else:
            # function
            matched = re.search(functionRegex, stdout, re.MULTILINE)
            if matched:
                result['what'] = 'function'
                result['type'] = matched.group('type')
                return result
    return None

def ghci_info_symbol(module_name, symbol_name):
    r = ghci_info(module_name, symbol_name)
    if not r:
        return None
    if 'what' not in r:
        return None
    if r['what'] == 'function':
        return symbols.Function(r['name'], r['type'])
    elif r['what'] == 'class':
        return symbols.Class(r['name'], r['ctx'] if 'ctx' in r else None, r['args'])
    elif r['what'] == 'data':
        return symbols.Data(r['name'], r['ctx'] if 'ctx' in r else None, r['args'])
    elif r['what'] == 'type':
        return symbols.Type(r['name'], r['ctx'] if 'ctx' in r else None, r['args'])
    elif r['what'] == 'newtype':
        return symbols.Newtype(r['name'], r['ctx'] if 'ctx' in r else None, r['args'])
    else:
        return None
