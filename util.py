# -*- coding: UTF-8 -*-

import sublime

if int(sublime.version()) < 3000:
    import ghci
    import ghcmod
    import haskell_docs
    import hdevtools
    import sublime_haskell_common as common
else:
    import SublimeHaskell.ghci as ghci
    import SublimeHaskell.ghcmod as ghcmod
    import SublimeHaskell.haskell_docs as haskell_docs
    import SublimeHaskell.hdevtools as hdevtools
    import SublimeHaskell.sublime_haskell_common as common

def symbol_info(filename, module_name, symbol_name, cabal = None, no_ghci = False):
    result = None
    if hdevtools.hdevtools_enabled():
        result = hdevtools.hdevtools_info(filename, symbol_name, cabal = cabal)
    if not result and ghcmod.ghcmod_enabled():
        result = ghcmod.ghcmod_info(filename, module_name, symbol_name, cabal = cabal)
    if not result and not filename and not no_ghci:
        result = ghci.ghci_info(module_name, symbol_name, cabal = cabal)
    return result

def load_docs(decl):
    """
    Tries to load docs for decl
    """
    if decl.docs is None:
        decl.docs = haskell_docs.haskell_docs(decl.module.name, decl.name)

def refine_type(decl, no_ghci = True):
    """
    Refine type for sources decl
    """
    if decl.location:
        if decl.what == 'function' and not decl.type:
            info = symbol_info(decl.location.filename, decl.module.name, decl.name, None, no_ghci = no_ghci)
            if info:
                decl.type = info.type

def refine_decl(decl):
    """
    Refine decl information.
    """
    # Symbol from cabal, try to load detailed info with ghci
    if not decl.location:
        load_docs(decl)

        if decl.what == 'declaration':
            decl_detailed = ghci.ghci_info(decl.module.name, decl.name)
            if decl_detailed:
                decl.__dict__.update(decl_detailed.__dict__)

    # Symbol from sources, concrete type if it's not specified
    else:
        refine_type(decl, False)

def browse_module(module_name, cabal = None):
    """
    Returns symbols.Module with all declarations
    """
    return ghcmod.ghcmod_browse_module(module_name, cabal = cabal)
