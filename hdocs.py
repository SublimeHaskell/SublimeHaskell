import sublime
import json
import time

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
else:
    from SublimeHaskell.sublime_haskell_common import *

def call_hdocs_and_wait(args, filename = None, cabal = None):
    ghc_opts_args = get_ghc_opts_args(filename, cabal = cabal)
    source_dir = get_source_dir(filename)
    
    try:
        command = ['hdocs'] + args + ghc_opts_args
        log(command)

        exit_code, out, err = call_and_wait(command, cwd = source_dir)

        if exit_code != 0:
            raise Exception("hdocs exited with status %d and stderr: %s" % (exit_code, err))

        return crlf2lf(out)

    except OSError as e:
        if e.errno == errno.ENOENT:
            output_error(sublime.active_window(), "SublimeHaskell: hdocs was not found!")

        return None

    except Exception as e:
        log('calling to hdocs fails with {0}'.format(e))
        return None

def module_docs(module_name, cabal = None):
    contents = call_hdocs_and_wait(['docs', module_name], cabal = cabal)
    return json.loads(contents)

def symbol_docs(module_name, symbol_name, cabal = None):
    contents = call_hdocs_and_wait(['docs', module_name, symbol_name], cabal = cabal)
    return contents

def load_module_docs(module):
    if module.location:
        return False
    if 'hdocs' in module.tags:
        return False

    docs = module_docs(module.name, module.cabal)
    module.tags['hdocs'] = time.clock()
    for decl in module.declarations.values():
        if decl.name in docs:
            decl.docs = docs[decl.name]

    return True
