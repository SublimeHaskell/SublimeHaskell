"""
The ghc-mod backend
"""

import io
import os.path
import pprint
import re
import threading
import sys

import sublime

# import SublimeHaskell.internals.regexes as Regexes
import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.which as Which
import SublimeHaskell.symbols as symbols

FILE_LINE_COL_REGEX = r'\s*^(?P<file>\S*):(?P<line>\d+):(?P<col>\d+):(\s*(?P<flag>\*|[Ww]arning:)\s+)?'
GHC_CHECK_REGEX = re.compile(FILE_LINE_COL_REGEX + r'(?P<details>.*$(\n^(?:\*?\s+).*$)*)',
                             re.MULTILINE)
GHC_LINT_REGEX = re.compile(FILE_LINE_COL_REGEX + r'(?P<msg>.*$)(?P<details>(\n(.*$))+)',
                            re.MULTILINE)

class GHCModBackend(Backend.HaskellBackend):
    '''This class encapsulates all of the functions that interact with the `ghc-mod` backend.
    '''

    def __init__(self, backend_mgr, **kwargs):
        super().__init__(backend_mgr)
        exec_with = kwargs.get('exec-with')
        install_dir = kwargs.get('install-dir')

        if exec_with is not None:
            if install_dir is None:
                sublime.error_message('\n'.join(['\'exec_with\' requires an \'install_dir\'.',
                                                 '',
                                                 'Please check your \'backends\' configuration and retry.']))
                raise RuntimeError('\'exec_with\' requires an \'install_dir\'.')
            elif exec_with not in ['stack', 'cabal']:
                sublime.error_message('\n'.join(['Invalid backend \'exec_with\': {0}'.format(exec_with),
                                                 '',
                                                 'Valid values are "cabal" or "stack".',
                                                 'Please check your \'backends\' configuration and retry.']))
                raise RuntimeError('Invalid backend \'exec_with\': {0}'.format(exec_with))

        self.exec_with = exec_with
        self.install_dir = install_dir

        # The project backends, indexed by project name
        self.project_backends = {}

    @staticmethod
    def backend_name():
        return 'ghc-mod'

    @staticmethod
    def is_available(**_kwargs):
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
            self.project_backends[project].shutdown()
        self.project_backends = {}

    def is_live_backend(self):
        '''ghc-mod is always a live backend.'''
        return True

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # File/project tracking functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def add_project_file(self, filename, project, project_dir):
        '''ghc-mod has to execute in the same directory as the project (and it's cabal file). Consequently, there will be
        multiple ghc-mod's executing when there are multiple projects open.
        '''
        super().add_project_file(filename, project, project_dir)

        # print('{0}.add_project_file: {1} {2} {3}'.format(type(self).__name__, filename, project, project_dir))
        if project not in self.project_backends:
            opt_args = self.get_ghc_opts_args(filename, add_package_db=True, cabal=project_dir)
            self.project_backends[project] = GHCModClient(project, project_dir, opt_args)

    def remove_project_file(self, filename):
        pass

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API/action functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def ping(self):
        return True

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False, **backend_args):
        # print('ghc-mod scan: cabal {0} sandboxes {1} projects {2} files {3} paths {4} ghc {5} contents {6}'.format(
        #     cabal, sandboxes, projects, files, paths, ghc, contents))
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
        # Yes, I know. This is gratuitous. But clear in what is intended.
        return super().list_projects(**backend_args)

    def symbol(self, lookup='', search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def module(self, project_name, lookup='', search_type='prefix', project=None, file=None, module=None, deps=None,
               sandbox=None, cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
        modsyms = None
        if search_type == 'exact' and re.match(r'\w+(\.\w+)+', lookup):
            backend = self.project_backends.get(project_name)
            modinfo, err = backend.command_backend('browse -d -o ' + lookup) if backend is not None else []
            if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
                print('ghc-mod modules: resp =\n{0}'.format(pprint.pformat(modinfo)))

            if not err or 'EXCEPTION' not in ' '.join(err):
                moddecls = {}
                for mdecl in modinfo:
                    decl = None
                    name, declinfo = self.get_name_decl(mdecl)
                    if declinfo.startswith('class '):
                        ctx, args = self.split_context_args(name, declinfo[5:])
                        decl = symbols.Class(name, ctx, args)
                    elif declinfo.startswith('data '):
                        ctx, args = self.split_context_args(name, declinfo[5:])
                        decl = symbols.Data(name, ctx, args)
                    elif declinfo.startswith('newtype '):
                        ctx, args = self.split_context_args(name, declinfo[8:])
                        decl = symbols.Newtype(name, ctx, args)
                    elif declinfo.startswith('type '):
                        ctx, args = self.split_context_args(name, declinfo[5:])
                        decl = symbols.Type(name, ctx, args)
                    else:
                        # Default to function
                        decl = symbols.Function(name, declinfo)

                    if decl is not None:
                        moddecls[name] = decl

                if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
                    print('ghc-mod modules: moddecls =\n{0}'.format(pprint.pformat(moddecls)))

                modsyms = symbols.Module(lookup, [], [], moddecls, symbols.PackageDb(global_db=True))

        return self.dispatch_callbacks([modsyms] if modsyms else [], **backend_args)

    def resolve(self, file, exports=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def project(self, project=None, path=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def sandbox(self, path, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def lookup(self, name, file, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def whois(self, name, file, **backend_args):
        # backend = self.project_backends.get(project_name)
        return self.dispatch_callbacks([], **backend_args)

    def scope_modules(self, project_name, _filename, lookup='', search_type='prefix', **backend_args):
        def make_pkg(pkg):
            pkg_info = pkg.split('-', 2)
            pkg_name = pkg_info[0]
            if len(pkg_info) > 0:
                pkg_ver = pkg_info[1]
            else:
                pkg_ver = '<no version>'

            return symbols.Package(pkg_name, pkg_ver)

        backend = self.project_backends.get(project_name)
        modules, _ = backend.command_backend('list -d') if backend is not None else ([], [])
        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod scope_modules: resp =\n{0}'.format(modules))

        filtered_mods = [symbols.Module(mod[1], [], [], {},
                                        symbols.InstalledLocation(make_pkg(mod[0]), symbols.PackageDb(global_db=True)))
                         for mod in (m.split() for m in modules if self.lookup_match(m[1], lookup, search_type))]

        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod scope_modules: filtered_mods\n{0}'.format(pprint.pformat(filtered_mods)))

        return self.dispatch_callbacks(filtered_mods, **backend_args)

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def complete(self, lookup, file, wide=False, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def hayoo(self, query, page=None, pages=None, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def cabal_list(self, packages, **backend_args):
        return self.dispatch_callbacks([], **backend_args)

    def lint(self, files=None, contents=None, hlint=None, wait_complete=False, **backend_args):
        lint_cmd = ' '.join(['lint', '--hlintOpt', '-u'] + (hlint or []))
        lint_output = self.translate_regex_output(lint_cmd, files, contents, GHC_LINT_REGEX, self.translate_lint)
        return self.dispatch_callbacks(lint_output, **backend_args)

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        check_cmd = 'check '
        check_output = self.translate_regex_output(check_cmd, files, contents, GHC_CHECK_REGEX, self.translate_check)
        return self.dispatch_callbacks(check_output, **backend_args)

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=False, **backend_args):
        '''ghc-mod cannot generate corrections to autofix. Returns an empty list.
        '''
        return self.dispatch_callbacks([], **backend_args)

    def types(self, project_name, file, module_name, line, column, ghc_flags=None, contents=None, **backend_args):
        type_output = []
        if contents is not None and file[0] in contents:
            map_file = True
            fcontent = contents[file[0]]
        else:
            map_file = False
            fcontent = None

        # Convert 0-based to 1-based line/col:
        type_cmd = 'type {0} {1} {2}'.format(file[0], line + 1, column + 1)
        if Settings.COMPONENT_DEBUG.send_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod types: type_cmd \'{0}\''.format(type_cmd))

        result, _ = self.command_backend(file[0], type_cmd, map_file, file[0], fcontent)

        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod types: type_output\n{0}'.format(pprint.pformat(result)))


        for tyline in result:
            line = tyline.split(maxsplit=4)
            type_output.append({'note': {'expr': '',
                                         'type': line[4].replace('"', '')},
                                'source': {},
                                'region': {'from': {'line': line[0],
                                                    'column': line[1]},
                                           'to': {'line': line[2],
                                                  'column': line[3]}},
                                'level': None})
        return self.dispatch_callbacks(type_output, **backend_args)

    def langs(self, project_name, **backend_args):
        backend = self.project_backends.get(project_name)
        langs, _ = backend.command_backend('lang') if backend is not None else []
        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod langs: resp =\n{0}'.format(langs))
        return self.dispatch_callbacks(langs, **backend_args)

    def flags(self, project_name, **backend_args):
        backend = self.project_backends.get(project_name)
        flags, _ = backend.command_backend('flag') if backend is not None else []
        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod langs: resp =\n{0}'.format(flags))
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
    # Advanced features:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def query_import(self, _symbol, _filename):
        return (False, ['ghc-mod does not support query_import used by \'Add Import\''])

    def contents_to_module(self, contents):
        return None

    def clean_imports(self, filename):
        return (False, ['NullBackend does not support the clean_imports functionality'])

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Utility functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def get_project_dir(self, filename):
        retval = None
        backend_info = self.file_to_project.get(filename)
        if backend_info is not None:
            retval = backend_info[1]

        return retval

    def get_backend(self, filename):
        backend_info = self.file_to_project.get(filename)
        if backend_info is not None:
            backend = self.project_backends.get(backend_info[0])
            if backend is not None:
                return backend
            else:
                Logging.log('{0}: {1} does not have an active ghc-mod!'.format(type(self).__name__, backend_info[0]))
        else:
            Logging.log('{0}: {1} does not map to a project!'.format(type(self).__name__, filename))

        return None

    def command_backend(self, filename, cmd, do_map=False, file=None, contents=None):
        backend = self.get_backend(filename)
        if backend is not None:
            return backend.command_backend(cmd, do_map, file, contents)
        else:
            return ([], [])

    def translate_regex_output(self, cmd, files, contents, regex, xlat_func):
        retval = []
        for file in files:
            project_dir = self.get_project_dir(file)
            if contents is not None and file in contents:
                map_file = True
                fcontent = contents[file]
            else:
                map_file = False
                fcontent = None
            resp, _ = self.command_backend(file, cmd + ' ' + file, map_file, file, fcontent)
            if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
                print('ghc-mod: {0}: resp =\n{1}'.format(cmd, pprint.pformat(resp)))
            retval.extend([xlat_func(project_dir, m) for m in regex.finditer('\n'.join(resp))])

        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod: {0}:\n{1}'.format(cmd, pprint.pformat(retval)))

        return retval

    def translate_check(self, project_dir, errmsg):
        line, column = int(errmsg.group('line')), int(errmsg.group('col'))

        # HACK ALERT: If the file name is not absolute, that means ghc-mod reported it relative to the
        # project directory. So we have to reconstitute the full file name expected by SublimeHaskell.
        filename = errmsg.group('file')
        if not os.path.isabs(filename):
            filename = os.path.normpath(os.path.join(project_dir, filename))

        flag = errmsg.group('flag')
        level_type = 'error' if flag is None or not flag.lower().startswith('warning') else 'warning'

        return {'level': level_type,
                'note': {'message': errmsg.group('details'), 'suggestions': None},
                'region': {'from': {'column': 1, 'line': line},
                           'to': {'column': column, 'line': line}},
                'source': {'file': filename,
                           'project': None}
               }

    def translate_lint(self, project_dir, errmsg):
        line, column = int(errmsg.group('line')), int(errmsg.group('col'))

        # HACK ALERT: If the file name is not absolute, that means ghc-mod reported it relative to the
        # project directory. So we have to reconstitute the full file name expected by SublimeHaskell.
        filename = errmsg.group('file')
        if not os.path.isabs(filename):
            filename = os.path.normpath(os.path.join(project_dir, filename))

        # ghc-mod does not return the start and end of the region, so we can't craft a corrector.
        return {'level': 'hint',
                'note': {'message': '{0}{1}'.format(errmsg.group('msg'), errmsg.group('details')),
                         'suggestions': None},
                'region': {'from': {'column': 1, 'line': line},
                           'to': {'column': column, 'line': line}},
                'source': {'file': filename,
                           'project': None}
               }

    def lookup_match(self, elt, lookup, search_type):
        ## Note: The tests are ordered from most to least likely. In fact, I'm not sure if infix or regex is actually
        ## used in the code.
        return (search_type == 'exact' and elt == lookup) or \
               (search_type == 'prefix' and elt.startswith(lookup)) or \
               (search_type == 'suffix' and elt.endswith(lookup)) or \
               (search_type == 'infix' and lookup in elt) or \
               (search_type == 'regex' and re.search(lookup, elt))

    def split_context_args(self, name, signature):
        def trim_name(args):
            return args[1:] if args[0] == name else args

        sig = signature.split(' => ')
        if len(sig) == 1:
            return (None, trim_name(sig[0].split()))
        else:
            return (sig[0], trim_name(sig[1].split()))

    def get_name_decl(self, signature):
        sig = signature.split(' :: ')
        return (sig[0], sig[1] if len(sig) > 1 else '')

    def ghci_package_db(self, cabal):
        if cabal is not None and cabal != 'cabal':
            package_conf = [pkg for pkg in os.listdir(cabal) if re.match(r'packages-(.*)\.conf', pkg)]
            if package_conf:
                return os.path.join(cabal, package_conf)

        return None


    def get_ghc_opts(self, filename, add_package_db, cabal):
        """
        Gets ghc_opts, used in several tools, as list with extra '-package-db' option and '-i' option if filename passed
        """
        ghc_opts = Settings.PLUGIN.ghc_opts or []
        if add_package_db:
            package_db = self.ghci_package_db(cabal=cabal)
            for pkgdb in package_db or []:
                ghc_opts.append('-package-db {0}'.format(pkgdb))

        if filename:
            ghc_opts.append('-i {0}'.format(ProcHelper.get_source_dir(filename)))

        return ghc_opts


    def get_ghc_opts_args(self, filename, add_package_db, cabal):
        """
        Same as ghc_opts, but uses '-g' option for each option
        """
        opts = self.get_ghc_opts(filename, add_package_db, cabal)
        args = []
        for opt in opts:
            args.extend(['-g', opt])
        return args

    ## Unreferenced function:
    ##
    # def ghcmod_info(filename, module_name, symbol_name, cabal=None):
    #     """
    #     Uses ghc-mod info filename module_name symbol_name to get symbol info
    #     """
    #     contents = call_ghcmod_and_wait(['info', filename, module_name, symbol_name], filename=filename, cabal=cabal)
    #     # TODO: Returned symbol doesn't contain location
    #     # But in fact we use ghcmod_info only to retrieve type of symbol
    #     return ParseOutput.parse_info(symbol_name, contents)

class GHCModClient(object):
    ## Have ghc-mod prefix output with X's and O's (errors and regular output)
    ## Apologies to Ellie King. :-)
    GHCMOD_OUTPUT_MARKER = 'O: '
    GHCMOD_ERROR_MARKER = 'X: '

    def __init__(self, project, project_dir, opt_args):
        Logging.log('Starting \'ghc-mod\' for project {0}'.format(project), Logging.LOG_INFO)

        self.ghcmod = None
        self.action_lock = None
        self.stderr_drain = None
        self.cmd = []
        self.diag_prefix = 'ghc-mod ' + project

        # if self.exec_with is not None:
        #     if self.exec_with == 'cabal':
        #         cmd += ['cabal']
        #     elif self.exec_with == 'stack':
        #         cmd += ['stack']

        self.cmd += ['ghc-mod']

        # if self.exec_with is not None:
        #     cmd += ['--']

        self.cmd += ['-b', '\\n', '--line-prefix', self.GHCMOD_OUTPUT_MARKER + ',' + self.GHCMOD_ERROR_MARKER]
        self.cmd += opt_args
        self.cmd += ['legacy-interactive']

        Logging.log('ghc-mod command: {0}'.format(self.cmd), Logging.LOG_DEBUG)

        self.ghcmod = ProcHelper.ProcHelper(self.cmd, cwd=project_dir)
        if self.ghcmod.process is not None:
            self.ghcmod.process.stdin = io.TextIOWrapper(self.ghcmod.process.stdin, 'utf-8')
            self.ghcmod.process.stdout = io.TextIOWrapper(self.ghcmod.process.stdout, 'utf-8')
            self.action_lock = threading.Lock()
            self.stderr_drain = OutputCollector.DescriptorDrain(self.diag_prefix, self.ghcmod.process.stderr)
            self.stderr_drain.start()
        else:
            Logging.log('Did not start ghc-mod successfully.')

    def shutdown(self):
        if self.ghcmod is not None:
            try:
                print('', file=self.ghcmod.process.stdin, flush=True)
            except OSError:
                pass
        if self.stderr_drain is not None and self.stderr_drain.is_alive():
            self.stderr_drain.stop()
            self.stderr_drain.join()

        self.ghcmod = None
        self.action_lock = None
        self.stderr_drain = None

    def read_response(self):
        resp_stdout = []
        resp_stderr = []
        try:
            got_reply = False
            while not got_reply:
                resp = self.ghcmod.process.stdout.readline()
                if resp == '':
                    # EOF???
                    got_reply = True
                else:
                    prefix = resp[0:3]
                    resp = resp.rstrip()[3:]
                    if prefix == self.GHCMOD_OUTPUT_MARKER:
                        if resp == 'OK':
                            got_reply = True
                        else:
                            resp_stdout.append(resp.rstrip())
                    elif prefix == self.GHCMOD_ERROR_MARKER:
                        Logging.log('{0}: {1}'.format(self.diag_prefix, resp))
                        resp_stderr.append(resp)
                    elif prefix == 'NG ':
                        sys.stdout.write('Error response: ' + resp)
                        got_reply = True
                    else:
                        sys.stdout.write('Unexpected reply from ghc-mod client: ' + resp)
                        got_reply = True
        except OSError:
            self.shutdown()

        return (resp_stdout, resp_stderr)

    def command_backend(self, cmd, do_map=False, file=None, contents=None):
        with self.action_lock:
            try:
                if do_map:
                    print('map-file ' + file, file=self.ghcmod.process.stdin)
                    self.ghcmod.process.stdin.write(contents)
                    self.ghcmod.process.stdin.write('\n' + chr(4) + '\n')
                    self.ghcmod.process.stdin.flush()
                    self.read_response()
                try:
                    print(cmd, file=self.ghcmod.process.stdin, flush=True)
                    return self.read_response()
                except OSError:
                    self.shutdown()
                    return ([], [])
                finally:
                    if do_map:
                        print('unmap-file ' + file, file=self.ghcmod.process.stdin, flush=True)
                        self.read_response()
            except OSError:
                self.shutdown()
                return ([], [])
