"""
The ghc-mod backend
"""

import io
import os.path
import pprint
import subprocess

import sublime

import SublimeHaskell.ghcimod.ghcmod_ops as GHCIMod
import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.regexes as Regexes
import SublimeHaskell.internals.which as Which

class GHCModBackend(Backend.HaskellBackend):
    """This class encapsulates all of the functions that interact with the `hsdev` backend.
    """

    ## Have ghc-mod mark output with X's and O's (errors and regular output)
    ## Apologies to Ellie King. :-)
    GHCMOD_OUTPUT_MARKER = 'O: '
    GHCMOD_ERROR_MARKER = 'X: '

    def __init__(self, backend_mgr, **kwargs):
        super().__init__(backend_mgr)
        exec_with = kwargs.get('exec-with')
        install_dir = kwargs.get('install-dir')

        if exec_with is not None and install_dir is None:
            sublime.error_message('\n'.join(['\'exec_with\' requires an \'install_dir\'.',
                                             '',
                                             'Please check your \'backends\' configuration and retry.']))
            raise RuntimeError('\'exec_with\' requires an \'install_dir\'.')
        elif exec_with is not None and exec_with not in ['stack', 'cabal']:
            sublime.error_message('\n'.join(['Invalid backend \'exec_with\': {0}'.format(exec_with),
                                             '',
                                             'Valid values are "cabal" or "stack".',
                                             'Please check your \'backends\' configuration and retry.']))
            raise RuntimeError('Invalid backend \'exec_with\': {0}'.format(exec_with))

        self.exec_with = exec_with
        self.install_dir = install_dir

        # The project backends, indexed by project name
        self.project_backends = {}
        # The file to project mapping (gets us to the backend)
        self.file_to_backend = {}

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
        # Yup. A single blank line terminates ghc-mod legacy-interactive.
        for project in self.project_backends:
            print('', file=self.project_backends[project].process.stdout)
        self.project_backends = {}

    def is_live_backend(self):
        '''The NullHaskellBackend is never alive.
        '''
        return True

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # File/project tracking functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def add_project_file(self, filename, project, project_dir):
        print('{0}.add_project_file: {1} {2} {3}'.format(type(self).__name__, filename, project, project_dir))
        if project not in self.project_backends:
            Logging.log('Starting \'ghc-mod\' for project {0}'.format(project), Logging.LOG_INFO)

            cmd = []

            # if self.exec_with is not None:
            #     if self.exec_with == 'cabal':
            #         cmd += ['cabal']
            #     elif self.exec_with == 'stack':
            #         cmd += ['stack']

            cmd += ['ghc-mod']

            # if self.exec_with is not None:
            #     cmd += ['--']

            cmd += ['-b', '\\n', '--line-prefix', self.GHCMOD_OUTPUT_MARKER + ',' + self.GHCMOD_ERROR_MARKER]
            cmd += GHCIMod.get_ghc_opts_args(filename, add_package_db=True, cabal=project_dir)
            cmd += ['legacy-interactive']

            Logging.log('ghc-mod command: {0}'.format(cmd), Logging.LOG_DEBUG)

            proc = ProcHelper.ProcHelper(cmd, stderr=subprocess.STDOUT, cwd=project_dir)
            if proc.process is not None:
                proc.process.stdin = io.TextIOWrapper(proc.process.stdin, 'utf-8')
                proc.process.stdout = io.TextIOWrapper(proc.process.stdout, 'utf-8')
                self.project_backends[project] = proc

        if filename not in self.file_to_backend:
            self.file_to_backend[filename] = (project, project_dir)

    def remove_project_file(self, filename, project, project_dir):
        pass

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
        project_info = []
        for pinfo in self.file_to_backend.values():
            if pinfo not in project_info:
                project_info.append(pinfo)
        return self.dispatch_callbacks([dict([('name', pinfo[0]), ('path', pinfo[1])]) for pinfo in project_info],
                                       **backend_args)

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
        def to_error(project_dir, errmsg):
            filename, line, column, _flag, messy_details = errmsg.groups()
            line, column = int(line), int(column)

            # HACK ALERT: If the file name is not absolute, that means ghc-mod reported it relative to the
            # project directory. So we have to reconstitute the full file name expected by SublimeHaskell.
            if not os.path.isabs(filename):
                filename = os.path.normpath(os.path.join(project_dir, filename))

            return {'level': 'hint',
                    'note': {'message': messy_details, 'suggestions': None},
                    'region': {'from': {'column': 1, 'line': line},
                               'to': {'column': column, 'line': line}},
                    'source': {'file': filename,
                               'project': None}
                   }

        lint_output = []
        for file in files:
            project_dir = self.get_project_dir(file)
            resp, _ = self.command_backend(file, 'lint ' + file)
            matches = Regexes.GHC_OUTPUT_REGEX.finditer('\n'.join(resp))
            lint_output.extend([to_error(project_dir, m) for m in matches])

        return self.dispatch_callbacks(lint_output, **backend_args)

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        def to_error(project_dir, errmsg):
            filename, line, column, flag, messy_details = errmsg.groups()
            line, column = int(line), int(column)

            # HACK ALERT: If the file name is not absolute, that means ghc-mod reported it relative to the
            # project directory. So we have to reconstitute the full file name expected by SublimeHaskell.
            if not os.path.isabs(filename):
                filename = os.path.normpath(os.path.join(project_dir, filename))

            level_type = 'error' if flag is None or not flag.lower().startswith('warning') else 'warning'

            return {'level': level_type,
                    'note': {'message': messy_details, 'suggestions': None},
                    'region': {'from': {'column': 1, 'line': line},
                               'to': {'column': column, 'line': line}},
                    'source': {'file': filename,
                               'project': None}
                   }

        check_output = []
        for file in files:
            project_dir = self.get_project_dir(file)
            resp, _ = self.command_backend(file, 'check ' + file)
            matches = Regexes.GHC_OUTPUT_REGEX.finditer('\n'.join(resp))
            check_output.extend([to_error(project_dir, m) for m in matches])

        # pprint.pprint(check_output)
        return self.dispatch_callbacks(check_output, **backend_args)

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

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Utility functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def get_backend(self, filename):
        retval = None
        backend_info = self.file_to_backend.get(filename)
        if backend_info is not None:
            retval = self.project_backends.get(backend_info[0])

        return retval

    def get_project_dir(self, filename):
        retval = None
        backend_info = self.file_to_backend.get(filename)
        if backend_info is not None:
            retval = backend_info[1]

        return retval

    def command_backend(self, filename, cmd):
        resp_stdout = []
        resp_stderr = []
        try:
            backend = self.get_backend(filename)
            if backend is not None:
                print(cmd, file=backend.process.stdin, flush=True)
                done = False
                while not done:
                    resp = backend.process.stdout.readline()
                    if resp == '':
                        # EOF???
                        done = True
                    else:
                        prefix = resp[0:3]
                        resp = resp.rstrip()[3:]
                        if prefix == self.GHCMOD_OUTPUT_MARKER:
                            if resp == 'OK':
                                done = True
                            else:
                                resp_stdout.append(resp.rstrip())
                        elif prefix == self.GHCMOD_ERROR_MARKER:
                            resp_stderr.append(resp.rstrip())
        except OSError:
            ## FIXME: Needs to do something clever here to cleanup on error
            pass

        return (resp_stdout, resp_stderr)
