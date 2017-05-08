# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell backend management class and helpers
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

# [SublimeLinter pylint-disable:"W0613"]

import SublimeHaskell.internals.utils as Utils

class HaskellBackend(object):
    '''Base class for SublimeHaskell backends. Provides the basic interface for managing and communicating with a
    backend (hsdev, hdevtools, ghc-mod).
    '''

    def __init__(self):
        super().__init__()

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Management functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    @staticmethod
    def backend_name():
        '''Return the backend's name, e.g. `hsdev` or `ghc-mod`.
        '''
        raise NotImplementedError("HaskellBackend.disconnect_backend needs an implementation.")

    @staticmethod
    def is_availabile():
        '''Test if the backend is available. For most backends, this verifies that an executable exists and possibly that the
        version is supported (see the `hsdev` version of this method.)

        Return True if the backend is available. The base class returns False.
        '''
        return False

    def start_backend(self):
        '''This method allows the `HaskellBackend` subclass to start a local process, if needed, with which it interacts.
        `hsdev`'s local backend option takes advantage of it.

        Returns `True` if backend startup was successful. If no local external process is created, just return 'True'.

        The `HaskellBackend` base class returns `False` to return the backend manager's state to BackendManager.INITIAL.
        You can't start up something that doesn't exist.
        '''
        return False

    def connect_backend(self):
        '''Once the backend has been started, this method is where the `HaskellBackend` subclass creates a connection to it.

        Returns 'True' if connected successfully to the backend.
        '''
        return False

    def disconnect_backend(self):
        '''Disconnect SublimeHaskell from the backend. In the case of `hsdev`, this disconnects the sockets. Does not
        return a value.
        '''
        raise NotImplementedError("HaskellBackend.disconnect_backend needs an implementation.")

    def stop_backend(self):
        '''Terminate the backend. In the case of a local `hsdev` server, this waits for the server to exit.
        '''
        raise NotImplementedError("HaskellBackend.stop_backend needs an implementation.")

    def is_live_backend(self):
        '''Determine if backend is alive and usable. Return True if alive and usable, otherwise False.
        '''
        raise NotImplementedError('HaskellBackend.is_alive_backend needs an implementation.')

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API/action functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def ping(self):
        return False

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False, **backend_args):
        raise NotImplementedError("HaskellBackend.scan needs an implementation.")

    def docs(self, projects=None, files=None, modules=None, **backend_args):
        raise NotImplementedError("HaskellBackend.docs needs an implementation.")

    def infer(self, projects=None, files=None, modules=None, **backend_args):
        raise NotImplementedError("HaskellBackend.infer needs an implementation.")

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None, **backend_args):
        raise NotImplementedError("HaskellBackend.remove needs an implementation.")

    def remove_all(self, **backend_args):
        raise NotImplementedError("HaskellBackend.remove_all needs an implementation.")

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False, **backend_args):
        raise NotImplementedError("HaskellBackend.list_modules needs an implementation.")

    def list_packages(self, **backend_args):
        raise NotImplementedError("HaskellBackend.list_packages needs an implementation.")

    def list_projects(self, **backend_args):
        raise NotImplementedError("HaskellBackend.list_projects needs an implementation.")

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False, **backend_args):
        raise NotImplementedError("HaskellBackend.symbol needs an implementation.")

    def module(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
        raise NotImplementedError("HaskellBackend.module needs an implementation.")

    def resolve(self, file, exports=False, **backend_args):
        raise NotImplementedError("HaskellBackend.resolve needs an implementation.")

    def project(self, project=None, path=None, **backend_args):
        raise NotImplementedError("HaskellBackend.project needs an implementation.")

    def sandbox(self, path, **backend_args):
        raise NotImplementedError("HaskellBackend.sandbox needs an implementation.")

    def lookup(self, name, file, **backend_args):
        raise NotImplementedError("HaskellBackend.lookup needs an implementation.")

    def whois(self, name, file, **backend_args):
        raise NotImplementedError("HaskellBackend.whois needs an implementation.")

    def scope_modules(self, file, lookup='', search_type='prefix', **backend_args):
        raise NotImplementedError("HaskellBackend.scope_modules needs an implementation.")

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        raise NotImplementedError("HaskellBackend.scope needs an implementation.")

    def complete(self, lookup, file, wide=False, **backend_args):
        raise NotImplementedError("HaskellBackend.complete needs an implementation.")

    def hayoo(self, query, page=None, pages=None, **backend_args):
        raise NotImplementedError("HaskellBackend.hayoo needs an implementation.")

    def cabal_list(self, packages, **backend_args):
        raise NotImplementedError("HaskellBackend.cabal_list needs an implementation.")

    def lint(self, files=None, contents=None, hlint=None, wait_complete=True, **backend_args):
        raise NotImplementedError("HaskellBackend.lint needs an implementation.")

    def check(self, files=None, contents=None, ghc=None, wait_complete=True, **backend_args):
        raise NotImplementedError("HaskellBackend.lint needs an implementation.")

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=True, **backend_args):
        raise NotImplementedError("HaskellBackend.check_lint needs an implementation.")

    def types(self, files=None, contents=None, ghc=None, **backend_args):
        raise NotImplementedError("HaskellBackend.types needs an implementation.")

    def langs(self, **backend_args):
        raise NotImplementedError("HaskellBackend.langs needs an implementation.")

    def flags(self, **backend_args):
        raise NotImplementedError("HaskellBackend.flags needs an implementation.")

    def autofix_show(self, messages, **backend_args):
        raise NotImplementedError("HaskellBackend.autofix_show needs an implementation.")

    def autofix_fix(self, messages, rest=None, pure=False, **backend_args):
        raise NotImplementedError("HaskellBackend.autofix_fix needs an implementation.")

    def ghc_eval(self, exprs, file=None, source=None, **backend_args):
        raise NotImplementedError("HaskellBackend.ghc_eval needs an implementation.")

    def exit(self):
        raise NotImplementedError("HaskellBackend.exit needs an implementation.")

# pylint: disable=W0613

class NullHaskellBackend(object, metaclass=Utils.Singleton):
    ''' For Haskellers: The Identity Backend. For ordinary mortals, this is the null, do-nothing Haskell backend. It does
    something sensible for all functions. The primary use case is to provide something sensible when no other backend is
    available or active.

    It is not recommended that any backend derive iteself from this class. Use it as a cheat sheet for what an API method
    should do -- sure.
    '''

    def __init__(self):
        super().__init__()

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Management functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    @staticmethod
    def backend_name():
        '''Return the backend's name, e.g. `hsdev` or `ghc-mod`.
        '''
        return "Null/Identity Backend"

    @staticmethod
    def is_availabile():
        return True

    def start_backend(self):
        return True

    def connect_backend(self):
        return True

    def disconnect_backend(self):
        pass

    def stop_backend(self):
        pass

    def is_live_backend(self):
        '''The NullHaskellBackend is never alive and usable.
        '''
        return False

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API/action functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def ping(self):
        return True

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False, **backend_args):
        return []

    def docs(self, projects=None, files=None, modules=None, **backend_args):
        return []

    def infer(self, projects=None, files=None, modules=None, **backend_args):
        return []

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None, **backend_args):
        return []

    def remove_all(self, **backend_args):
        return None

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False, **backend_args):
        return []

    def list_packages(self, **backend_args):
        return []

    def list_projects(self, **backend_args):
        return []

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False, **backend_args):
        return []

    def module(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
        return None

    def resolve(self, file, exports=False, **backend_args):
        return []

    def project(self, project=None, path=None, **backend_args):
        return []

    def sandbox(self, path, **backend_args):
        return []

    def lookup(self, name, file, **backend_args):
        return []

    def whois(self, name, file, **backend_args):
        return []

    def scope_modules(self, file, lookup='', search_type='prefix', **backend_args):
        return []

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        return []

    def complete(self, lookup, file, wide=False, **backend_args):
        return []

    def hayoo(self, query, page=None, pages=None, **backend_args):
        return []

    def cabal_list(self, packages, **backend_args):
        return []

    def lint(self, files=None, contents=None, hlint=None, wait_complete=False, **backend_args):
        return []

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        return []

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=False, **backend_args):
        return []

    def types(self, files=None, contents=None, ghc=None, **backend_args):
        return []

    def langs(self, **backend_args):
        return []

    def flags(self, **backend_args):
        return []

    def autofix_show(self, messages, **backend_args):
        return []

    def autofix_fix(self, messages, rest=None, pure=False, **backend_args):
        return []

    def ghc_eval(self, exprs, file=None, source=None, **backend_args):
        return []

    def exit(self):
        return True
