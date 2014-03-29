import sublime
import json
import time

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
else:
    from SublimeHaskell.sublime_haskell_common import *

def call_hdocs_and_wait(args, filename = None, cabal = None):
    ghc_opts_args = get_ghc_opts_args(filename, cabal = cabal)
    command = ['hdocs'] + args + ghc_opts_args
    log(command, log_trace)

    return call_and_wait_tool(command, 'hdocs', lambda o: json.loads(o), filename)

def module_docs(module_name, cabal = None):
    return call_hdocs_and_wait([module_name], cabal = cabal)

def symbol_docs(module_name, symbol_name, cabal = None):
    ret = call_hdocs_and_wait([module_name, symbol_name], cabal = cabal)
    if ret and symbol_name in ret:
        return ret[symbol_name]
    return None

def load_module_docs(module):
    if not hdocs_enabled():
        return False

    if module.location:
        return False
    if 'hdocs' in module.tags:
        return False

    docs = module_docs(module.name, module.cabal)
    if docs:
        module.tags['hdocs'] = time.clock()
        for decl in module.declarations.values():
            if decl.name in docs:
                decl.docs = docs[decl.name]
        return True

    return False

def hdocs_enabled():
    return get_setting_async('enable_hdocs') == True
