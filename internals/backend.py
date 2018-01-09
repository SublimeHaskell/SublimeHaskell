# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell backend management class and helpers
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

# [SublimeLinter pylint-disable:"W0613"]

class HaskellBackend(object):
    '''Base class for SublimeHaskell backends. Provides the basic interface for managing and communicating with a
    backend (hsdev, hdevtools, ghc-mod).
    '''

    def __init__(self, backend_mgr):
        super().__init__()
        self.backend_mgr = backend_mgr
        # The file to (project, project directory) mapping
        self.file_to_project = {}

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Management functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    @staticmethod
    def backend_name():
        '''Return the backend's name, e.g. `hsdev` or `ghc-mod`.
        '''
        raise NotImplementedError("HaskellBackend.backend_name needs an implementation.")

    @staticmethod
    def is_available(**_kwargs):
        '''Test if the backend is available. For most backends, this verifies that an executable exists and possibly that the
        version is supported (see the `hsdev` version of this method.)

        Return True if the backend is available. The base class returns False.
        '''
        return False

    def start_backend(self):
        '''Start the backend.

        This is the method where a subclass starts the backend, generally a local process with which SublimeHaskell
        communicates. A good example of this is the :py:class:`HsDevBackend` when it starts up a local *hsdev* process.

        :rtype: Boolean
        :return: `True` if backend startup was successful.

        The `HaskellBackend` base class returns `False`. You can't start up something that doesn't exist.
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
    # File/project tracking functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def add_project_file(self, filename, project, project_dir):
        if filename not in self.file_to_project:
            self.file_to_project[filename] = (project, project_dir)

    def remove_project_file(self, filename):
        self.file_to_project.pop(filename)

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
        '''Query the list of known projects.

        :rtype: List

        Each element in the returned list has the following structure::

            {'cabal': '<absolute path to .cabal file>',
             'description': {'executables': [{'info': {'build-depends': [<module dependencies>],
                                                       'extensions': [],
                                                       'ghc-options': [<ghc options>],
                                                       'language': 'Haskell2010',
                                                       'source-dirs': [<source directories>]},
                                              'name': '<project name>',
                                              'path': '<main haskell source file>'}],
                             'library': None,
                             'tests': [],
                             'version': '<version string>'},
             'name': '<project name>',
             'path': '<absolute path to top of the project>'}

        The *name* and *path* elements are mandatory. *description* should be collected from the Cabal file,
        but generally appears to be optional (SublimeHaskell doesn't use it at the moment, but the *hsdev*
        backend generates the info.)
        '''
        project_info = []
        for pinfo in self.file_to_project.values():
            if pinfo not in project_info:
                project_info.append(pinfo)

        project_list = [dict([('name', pinfo[0]), ('path', pinfo[1])]) for pinfo in project_info]
        return self.dispatch_callbacks(project_list, None, **backend_args)

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False, **backend_args):
        raise NotImplementedError("HaskellBackend.symbol needs an implementation.")

    def module(self, project_name, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None,
               sandbox=None, cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
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

    def scope_modules(self, project_name, file, lookup='', search_type='prefix', **backend_args):
        '''Get modules accessible from a module (source file) or within a project.

        :param str project_name: The project to interrogate
        :param str file: The source file to interrogate
        :param str lookup: The module name to query
        :param str search_type: How match the lookup: 'prefix', 'suffix', 'exact', 'infix' (contains), 'regex'
        :rtype: List of :py:class:`Module` symbol instances.
        '''
        raise NotImplementedError("HaskellBackend.scope_modules needs an implementation.")

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        raise NotImplementedError("HaskellBackend.scope needs an implementation.")

    def complete(self, sym, file, wide=False, **backend_args):
        '''Generate completions for a qualified symbol (:py:class:`QualifiedSymbol`)
        '''
        raise NotImplementedError("HaskellBackend.complete needs an implementation.")

    def hayoo(self, query, page=None, pages=None, **backend_args):
        raise NotImplementedError("HaskellBackend.hayoo needs an implementation.")

    def cabal_list(self, packages, **backend_args):
        raise NotImplementedError("HaskellBackend.cabal_list needs an implementation.")

    def lint(self, files=None, contents=None, hlint=None, wait_complete=True, **backend_args):
        '''Runs 'hlint' over a file (or its contents, if provided) and returns a list of suggestions with their
        locations in the source. Each suggestion is a dictionary with the following structure::

            {'level': 'hint',
             'note': {'corrector': {'contents': '<text to insert as a correction>',
                                    'region': {'from': {'column': 0, 'line': 21},
                                               'to': {'column': 25, 'line': 21}}},
                      'message': '<hint text>'},
             'region': {'from': {'column': 1, 'line': 22},
                        'to': {'column': 26, 'line': 22}},
             'source': {'file': '<absolute path to source>',
                        'project': None}}
        '''
        raise NotImplementedError("HaskellBackend.lint needs an implementation.")

    def check(self, files=None, contents=None, ghc=None, wait_complete=True, **backend_args):
        '''Runs the compiler over a file (or its contents, if provided) and returns a list of warnings and errors,
           as well as their locations in the source. Each error or warning has the following dictionary structure::

              [{'level': 'error',
                'note': {'message': "<message>", 'suggestion': None},
                'region': {'from': {'column': 15, 'line': 18},
                           'to': {'column': 23, 'line': 18}},
                 'source': {'file': '<absolute path to source>',
                            'project': None}},
                {'level': 'warning',
                 'note': {'message': "<message 2>", 'suggestion': None},
                 'region': {'from': {'column': 29, 'line': 22},
                            'to': {'column': 37, 'line': 22}},
                 'source': {'file': '<absolute path to source>',
                            'project': None}}]}
        '''
        raise NotImplementedError("HaskellBackend.lint needs an implementation.")

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=True, **backend_args):
        raise NotImplementedError("HaskellBackend.check_lint needs an implementation.")

    def types(self, project_name, file, module_name, line, column, ghc_flags=None, contents=None, **backend_args):
        raise NotImplementedError("HaskellBackend.types needs an implementation.")

    def langs(self, project_name, **backend_args):
        raise NotImplementedError("HaskellBackend.langs needs an implementation.")

    def flags(self, project_name, **backend_args):
        raise NotImplementedError("HaskellBackend.flags needs an implementation.")

    def autofix_show(self, messages, wait_complete, **backend_args):
        '''Show autofixes for errors, when possible. Can be used synchrnously or asynchronously.

        :param list(str) messages: A list of error messages
        :param bool wait_complete: If True, wait to receive a response from the backend.
        :return: The JSON autofix response from the backend, if :py:param:`wait_complete` is True, or None.
        '''
        raise NotImplementedError("HaskellBackend.autofix_show needs an implementation.")

    def autofix_fix(self, messages, rest=None, pure=False, **backend_args):
        raise NotImplementedError("HaskellBackend.autofix_fix needs an implementation.")

    def ghc_eval(self, exprs, file=None, source=None, **backend_args):
        raise NotImplementedError("HaskellBackend.ghc_eval needs an implementation.")

    def exit(self):
        raise NotImplementedError("HaskellBackend.exit needs an implementation.")

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Advanced features:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def query_import(self, symname, filename):
        '''Query possible import modules for symbol :py:var:`name` in the file named :py:var:`filename`.

        :rtype: (Boolean, List(String)) tuple
        :returns: If the first tuple element is True, the list of strings will be the module names from which
            :py:var:`name` can be imported. If False, the list of strings is an error message or diagnostic.
        '''
        raise NotImplementedError("HaskellBackend.add_import needs an implementation")

    def contents_to_module(self, contents):
        '''Convert Haskell source to a :py:class:`Module` object. This method is currently used to
        extract the imports when adding a missing import.

        :rtype: :py:class:`Module`
        :returns: A :py:class:`Module` object representing the Haskell source.
        '''
        raise NotImplementedError('HaskellBackend.contents_to_module needs an implementation')

    def clean_imports(self, filename):
        '''Clean the import list.

        :rtype: (Boolean, [string])
        :returns: If the backend supports this functionality, (True, [new imports]), otherwise (False, [error messages])
        '''
        raise NotImplementedError('HaskellBackend.clean_imports needs an implementation')

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Async dispatch functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def dispatch_callbacks(self, resp, errmsg, **kwargs):
        if not errmsg:
            on_response = kwargs.pop('on_response', None)
            return on_response(resp) if on_response is not None else resp
        else:
            on_error = kwargs.get('on_error', None)
            if isinstance(errmsg, list):
                errmsg = '\n'.join(errmsg)
            if on_error:
                on_error(self.backend_name(), errmsg)
            else:
                print('--- dispatching callbacks: error info:\n{0}\n-----'.format(errmsg))
            ## return on_error(errmsg) if on_error is not None else errmsg


class NullHaskellBackend(HaskellBackend):
    ''' For Haskellers: The Identity Backend. For ordinary mortals, this is the null, do-nothing Haskell backend. It does
    something sensible for all functions. The primary use case is to provide something sensible when no other backend is
    available or active.

    It is not recommended that any backend derive iteself from this class. Use it as a cheat sheet for what an API method
    should do.
    '''

    ## Uncomment if instance variables are needed.
    # def __init__(self, backend_mgr):
    #     super().__init__(backend_mgr)

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Management functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    @staticmethod
    def backend_name():
        '''Return the backend's name, e.g. `hsdev` or `ghc-mod`.
        '''
        return "none"

    @staticmethod
    def is_available(**_kwargs):
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
        '''The NullHaskellBackend is never alive.
        '''
        return False

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # File/project tracking functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    ## Uncomment if we do more than just super delegation.
    # def add_project_file(self, filename, project, project_dir):
    #     super().add_project_file(filename, project, project_dir)

    ## Uncomment if we do more than just super delegation.
    # def remove_project_file(self, filename):
    #     super().remove_project_file(filename)

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API/action functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def ping(self):
        return True

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def docs(self, projects=None, files=None, modules=None, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def infer(self, projects=None, files=None, modules=None, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def remove_all(self, **backend_args):
        return self.dispatch_callbacks(None, None, **backend_args)

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def list_packages(self, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    ## Uncomment if we do more than just super delegation.
    # def list_projects(self, **backend_args):
    #     return super().list_projects(**backend_args)

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def module(self, project_name, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None,
               sandbox=None, cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
        return self.dispatch_callbacks(None, None, **backend_args)

    def resolve(self, file, exports=False, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def project(self, project=None, path=None, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def sandbox(self, path, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def lookup(self, name, file, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def whois(self, name, file, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def scope_modules(self, project_name, file, lookup='', search_type='prefix', **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def complete(self, _sym, _file, wide=False, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def hayoo(self, query, page=None, pages=None, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def cabal_list(self, packages, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def lint(self, files=None, contents=None, hlint=None, wait_complete=False, **backend_args):
        return self.dispatch_callbacks(([], True), None, **backend_args)

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        return self.dispatch_callbacks(([], True), None, **backend_args)

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=False, **backend_args):
        return self.dispatch_callbacks(([], True), None, **backend_args)

    def types(self, project_name, file, module_name, line, column, ghc_flags=None, contents=None, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def langs(self, _projectname, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def flags(self, _projectname, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def autofix_show(self, messages, wait_complete, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def autofix_fix(self, messages, rest=None, pure=False, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def ghc_eval(self, exprs, file=None, source=None, **backend_args):
        return self.dispatch_callbacks([], None, **backend_args)

    def exit(self):
        return True

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Advanced features:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def query_import(self, symname, filename):
        '''Query possible import modules for symbol :py:var:`name` in the file named :py:var:`filename`.

        :rtype: (Boolean, List(String)) tuple
        :returns: If the first tuple element is True, the list of strings will be the module names from which
            :py:var:`name` can be imported. If False, the list of strings is an error message or diagnostic.
        '''
        return (False, ['NullBackend doe not support query_import used by \'Add Import\''])

    def contents_to_module(self, contents):
        return None

    def clean_imports(self, filename):
        return (False, ['NullBackend does not support the clean_imports functionality'])
