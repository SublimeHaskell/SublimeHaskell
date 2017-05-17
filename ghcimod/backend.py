"""
The ghc-mod backend
"""

import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.which as Which
import SublimeHaskell.ghcimod.ghci_backend as GHCIMod

class GHCModBackend(Backend.HaskellBackend):
    """This class encapsulates all of the functions that interact with the `hsdev` backend.
    """
    def __init__(self, backend_mgr):
        super().__init__(backend_mgr)

    @staticmethod
    def backend_name():
        return 'ghc-mod'

    @staticmethod
    def is_available():
        return Which.which('ghc-mod', ProcHelper.ProcHelper.get_extended_path())

    def start_backend(self):
        return True

    def connect_backend(self):
        return True

    def disconnect_backend(self):
        pass

    def stop_backend(self):
        pass

    def is_live_backend(self):
        '''The NullHaskellBackend is never alive.
        '''
        return False

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API/action functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def ping(self):
        return True

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def docs(self, projects=None, files=None, modules=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def infer(self, projects=None, files=None, modules=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def remove_all(self, **backend_args):
        return self.dispatch_callbacks(None, **backend_args)

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def list_packages(self, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def list_projects(self, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def module(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
        return self.dispatch_callbacks(None, **backend_args)

    def resolve(self, file, exports=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def project(self, project=None, path=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def sandbox(self, path, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def lookup(self, name, file, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def whois(self, name, file, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def scope_modules(self, file, lookup='', search_type='prefix', **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def complete(self, lookup, file, wide=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def hayoo(self, query, page=None, pages=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def cabal_list(self, packages, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def lint(self, files=None, contents=None, hlint=None, wait_complete=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def types(self, files=None, contents=None, ghc=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def langs(self, **backend_args):
        langs = GHCIMod.call_ghcmod_and_wait(['lang']).splitlines()
        return self.dispatch_callbacks(langs, **backend_args)

    def flags(self, **backend_args):
        flags = GHCIMod.call_ghcmod_and_wait(['flag']).splitlines()
        return self.dispatch_callbacks(flags, **backend_args)

    def autofix_show(self, messages, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def autofix_fix(self, messages, rest=None, pure=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def ghc_eval(self, exprs, file=None, source=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def exit(self):
        return True
