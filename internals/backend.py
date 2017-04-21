# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell backend management class and helpers
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

# [SublimeLinter pylint-disable:"W0613"]

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
        raise NotImplementedError("HaskellBackend.disconnect_backend needs an implementation.")

    def stop_backend(self):
        raise NotImplementedError("HaskellBackend.stop_backend needs an implementation.")

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API/action functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def ping(self):
        return False

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False):
        raise NotImplementedError("HaskellBackend.scan needs an implementation.")

    def docs(self, projects=None, files=None, modules=None):
        raise NotImplementedError("HaskellBackend.docs needs an implementation.")

    def infer(self, projects=None, files=None, modules=None):
        raise NotImplementedError("HaskellBackend.infer needs an implementation.")

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None):
        raise NotImplementedError("HaskellBackend.remove needs an implementation.")

    def remove_all(self):
        raise NotImplementedError("HaskellBackend.remove_all needs an implementation.")

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False):
        raise NotImplementedError("HaskellBackend.list_modules needs an implementation.")

    def list_packages(self):
        raise NotImplementedError("HaskellBackend.list_packages needs an implementation.")

    def list_projects(self):
        raise NotImplementedError("HaskellBackend.list_projects needs an implementation.")

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False):
        raise NotImplementedError("HaskellBackend.symbol needs an implementation.")

    def module(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False):
        raise NotImplementedError("HaskellBackend.module needs an implementation.")

    def resolve(self, file, exports=False):
        raise NotImplementedError("HaskellBackend.resolve needs an implementation.")

    def project(self, project=None, path=None):
        raise NotImplementedError("HaskellBackend.project needs an implementation.")

    def sandbox(self, path):
        raise NotImplementedError("HaskellBackend.sandbox needs an implementation.")

    def lookup(self, name, file):
        raise NotImplementedError("HaskellBackend.lookup needs an implementation.")

    def whois(self, name, file):
        raise NotImplementedError("HaskellBackend.whois needs an implementation.")

    def scope_modules(self, file, lookup='', search_type='prefix'):
        raise NotImplementedError("HaskellBackend.scope_modules needs an implementation.")

    def scope(self, file, lookup='', search_type='prefix', global_scope=False):
        raise NotImplementedError("HaskellBackend.scope needs an implementation.")

    def complete(self, lookup, file, wide=False):
        raise NotImplementedError("HaskellBackend.complete needs an implementation.")

    def hayoo(self, query, page=None, pages=None):
        raise NotImplementedError("HaskellBackend.hayoo needs an implementation.")

    def cabal_list(self, packages):
        raise NotImplementedError("HaskellBackend.cabal_list needs an implementation.")

    def lint(self, files=None, contents=None, hlint=None):
        raise NotImplementedError("HaskellBackend.lint needs an implementation.")

    def check(self, files=None, contents=None, ghc=None):
        raise NotImplementedError("HaskellBackend.lint needs an implementation.")

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None):
        raise NotImplementedError("HaskellBackend.check_lint needs an implementation.")

    def types(self, files=None, contents=None, ghc=None):
        raise NotImplementedError("HaskellBackend.types needs an implementation.")

    def langs(self):
        raise NotImplementedError("HaskellBackend.langs needs an implementation.")

    def flags(self):
        raise NotImplementedError("HaskellBackend.flags needs an implementation.")

    def autofix_show(self, messages):
        raise NotImplementedError("HaskellBackend.autofix_show needs an implementation.")

    def autofix_fix(self, messages, rest=None, pure=False):
        raise NotImplementedError("HaskellBackend.autofix_fix needs an implementation.")

    def ghc_eval(self, exprs, file=None, source=None):
        raise NotImplementedError("HaskellBackend.ghc_eval needs an implementation.")

    def exit(self):
        raise NotImplementedError("HaskellBackend.exit needs an implementation.")

# pylint: disable=W0613

class NullHaskellBackend(object):
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

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API/action functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def ping(self):
        return True

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False):
        return []

    def docs(self, projects=None, files=None, modules=None):
        return []

    def infer(self, projects=None, files=None, modules=None):
        return []

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None):
        return []

    def remove_all(self):
        return None

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False):
        return []

    def list_packages(self):
        return []

    def list_projects(self):
        return []

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False):
        return []

    def module(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False):
        return None

    def resolve(self, file, exports=False):
        return []

    def project(self, project=None, path=None):
        return []

    def sandbox(self, path):
        return []

    def lookup(self, name, file):
        return []

    def whois(self, name, file):
        return []

    def scope_modules(self, file, lookup='', search_type='prefix'):
        return []

    def scope(self, file, lookup='', search_type='prefix', global_scope=False):
        return []

    def complete(self, lookup, file, wide=False):
        return []

    def hayoo(self, query, page=None, pages=None):
        return []

    def cabal_list(self, packages):
        return []

    def lint(self, files=None, contents=None, hlint=None):
        return []

    def check(self, files=None, contents=None, ghc=None):
        return []

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None):
        return []

    def types(self, files=None, contents=None, ghc=None):
        return []

    def langs(self):
        return []

    def flags(self):
        return []

    def autofix_show(self, messages):
        return []

    def autofix_fix(self, messages, rest=None, pure=False):
        return []

    def ghc_eval(self, exprs, file=None, source=None):
        return []

    def exit(self):
        return True
