'''
The ghc-mod backend
'''

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
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.symbols as symbols

## The capturing version of the warning/error message
FILE_LINE_COL_REGEX = r'^(?P<file>\S*):(?P<line>\d+):(?P<col>\d+):\s*((?P<flag>[Ww]arning|\*|\w+):\s+)?'
## The non-capturing version, which stops the match at the next warning/error when spanning across multiple lines.
NONCAPTURE_FLC_REGEX = r'^\S*:\d+:\d+:\s*(\*|[Ww]arning|\w+):'

GHC_CHECK_REGEX = re.compile(FILE_LINE_COL_REGEX + r'(?P<details>.*$(\n?^(?!' + NONCAPTURE_FLC_REGEX + r')(?:\*?\s+).*$)*)',
                             re.MULTILINE)
GHC_LINT_REGEX = re.compile(FILE_LINE_COL_REGEX + r'(?P<msg>.*$)(?P<details>(\n?((?!' + NONCAPTURE_FLC_REGEX + r').*$))+)',
                            re.MULTILINE)

def debug_send():
    return Settings.COMPONENT_DEBUG.all_messages or Settings.COMPONENT_DEBUG.send_messages

def debug_recv():
    return Settings.COMPONENT_DEBUG.all_messages or Settings.COMPONENT_DEBUG.recv_messages

def debug_any():
    return debug_send() or debug_recv()

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
            elif exec_with not in ['stack', 'cabal', 'cabal-new-build']:
                sublime.error_message('\n'.join(['Invalid backend \'exec_with\': {0}'.format(exec_with),
                                                 '',
                                                 'Valid values are "cabal", "cabal-new-build" or "stack".',
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

        print('{0}.add_project_file: {1} {2} {3}'.format(type(self).__name__, filename, project, project_dir))
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

    scan = Backend.default_method_implementation()
    docs = Backend.default_method_implementation()
    infer = Backend.default_method_implementation()
    remove = Backend.default_method_implementation()
    remove_all = Backend.default_method_implementation()
    list_modules = Backend.default_method_implementation()
    list_packages = Backend.default_method_implementation()
    symbol = Backend.default_method_implementation()

    # Probably a little too explicit... useless call to super()
    # def list_projects(self, **backend_args):
    #     return super().list_projects(**backend_args)

    def module(self, project_name, lookup='', search_type='prefix', project=None, file=None, module=None, deps=None,
               sandbox=None, cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
        modsyms = None

        if search_type == 'exact' and re.match(r'\w+(\.\w+)+', lookup):
            backend = self.project_backends.get(project_name)
            modinfo = backend.command_backend('browse -d -o ' + lookup) if backend is not None else []
            if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
                print('ghc-mod modules: resp =\n{0}'.format(pprint.pformat(modinfo)))

            impinfo = [symbols.Module(lookup)]
            moddecls = dict([(decl.name, decl) for decl in [self.parse_syminfo(mdecl, impinfo) for mdecl in modinfo]])
            if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
                print('ghc-mod modules: moddecls =\n{0}'.format(pprint.pformat(moddecls)))

                modsyms = symbols.Module(lookup, [], [], moddecls, symbols.PackageDb(global_db=True))

        return self.dispatch_callbacks([modsyms] if modsyms else [], None, **backend_args)

    resolve = Backend.default_method_implementation()
    project = Backend.default_method_implementation()
    sandbox = Backend.default_method_implementation()
    lookup = Backend.default_method_implementation()
    whois = Backend.default_method_implementation()
    whoat = Backend.default_method_implementation()

    def scope_modules(self, project_name, _filename, lookup='', search_type='prefix', **backend_args):
        def make_pkg(pkg):
            pkg_info = pkg.split('-', 2)
            pkg_name = pkg_info[0]
            if pkg_info:
                pkg_ver = pkg_info[1]
            else:
                pkg_ver = '<no version>'

            return symbols.Package(pkg_name, pkg_ver)

        backend = self.project_backends.get(project_name)
        modules = backend.command_backend('list -d') if backend is not None else ([], [])
        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod scope_modules: resp =\n{0}'.format(modules))

        filtered_mods = [symbols.Module(mod[1], [], [], {},
                                        symbols.InstalledLocation(make_pkg(mod[0]), symbols.PackageDb(global_db=True)))
                         for mod in (m.split() for m in modules if self.lookup_match(m[1], lookup, search_type))]

        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod scope_modules: filtered_mods\n{0}'.format(pprint.pformat(filtered_mods)))

        return self.dispatch_callbacks(filtered_mods, None, **backend_args)

    scope = Backend.default_method_implementation()
    usages = Backend.default_method_implementation()

    ## Regular expressions used by the completions "logic"
    MODNAME_RE = r'^module\s+([A-Z]\w+)'
    MODNAME_MATCH = re.compile(MODNAME_RE)
    IMPORT_MATCH = re.compile(r'(?P<isqual>qualified\s+)?(?P<import>[A-Z]\w+(\.[A-Z]\w+)*)(?!\s*\()(.*\sas\s+(?P<qual>[A-Z][A-Za-z0-9\'_]*))')

    def complete(self, sym, file, wide=False, **backend_args):
        completions = []

        ## Have to do this the "old fashioned" way by looking at the actual file buffer, scraping for the module name,
        ## scraping for the imports, then collecting the names, which may or may not be visible.
        for view in filter(None, [w.find_open_file(file) for w in sublime.windows()]):
            ## Logging.log('ghc-mod complete: view = {0}'.format(view), Logging.LOG_DEBUG)

            _, project_name = Common.locate_cabal_project_from_view(view)
            backend = self.project_backends.get(project_name)

            ## Accumulate the source's module and imports:
            mod_dict = {}

            ## Symbols for the 'module'
            modname_region = view.find(self.MODNAME_RE, 0)
            if modname_region:
                modname = self.MODNAME_MATCH.search(view.substr(modname_region)).group(1)
                if modname:
                    mod_dict[modname] = (False, '')

            for imp_region in view.find_all(r'^import\s+(qualified\s+)'):
                imp_text = Common.get_line_contents(view, imp_region)
                ##print('imp_text \'{0}\''.format(imp_text))
                imp = self.IMPORT_MATCH.search(imp_text)
                import_name = imp.group('import')
                if imp and import_name and import_name not in mod_dict:
                    mod_dict[import_name] = (imp.group('isqual') is not None, imp.group('qual'))

            ## Adapted from the ghc-mod ghc-comp.el source: These modules are automagically 'preloaded' along with
            ## any other modules imported by the primary source module. Note that if the user already imports one of
            ## these modules explicitly, it won't get added.
            for preload in ['Prelude', 'Control.Applicative', 'Control.Exception', 'Control.Monad', 'Data.Char', 'Data.List',
                            'Data.Maybe', 'System.IO']:
                if preload not in mod_dict:
                    mod_dict[preload] = (False, None)

            completion_modules = [(modname,) + mod_dict[modname] for modname in mod_dict]

            ## 'sum' will flatten the list-of-lists created by the comprehension...
            if Settings.COMPONENT_DEBUG.completions:
                print('completion_modules:\n{0}\nunique-ified\n{1}'.format(pprint.pformat(completion_modules),
                                                                           pprint.pformat(list(set(completion_modules)))))

            completions = sum([self.collect_completions(backend, mod, sym.qualified_name())
                               for mod in list(set(completion_modules))], [])

        return self.dispatch_callbacks(filter(None, completions), None, **backend_args)

    hayoo = Backend.default_method_implementation()
    cabal_list = Backend.default_method_implementation()
    unresolveds = Backend.default_method_implementation()

    def lint(self, files=None, contents=None, hlint=None, wait_complete=False, **backend_args):
        lint_cmd = ' '.join(['lint'] + (hlint or []))
        lint_output = self.translate_regex_output(lint_cmd, files, contents, GHC_LINT_REGEX, self.translate_lint)
        return self.dispatch_callbacks(lint_output, None, **backend_args)

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        check_cmd = 'check '
        check_output = self.translate_regex_output(check_cmd, files, contents, GHC_CHECK_REGEX, self.translate_check)
        return self.dispatch_callbacks(check_output, None, **backend_args)

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=False, **backend_args):
        '''ghc-mod cannot generate corrections to autofix. Returns an empty list.
        '''
        return self.dispatch_callbacks([], None, **backend_args)

    def types(self, project_name, file, module_name, line, column, ghc_flags=None, contents=None, **backend_args):
        type_output = []
        if contents is not None and file[0] in contents:
            map_file = True
            fcontent = contents[file[0]]
        else:
            map_file = False
            fcontent = None

        ## Paranoia: Ensure line and column are integers.
        line = int(line)
        column = int(column)

        # Convert 0-based to 1-based line/col:
        type_cmd = 'type {0} {1} {2}'.format(file[0], line + 1, column + 1)
        if Settings.COMPONENT_DEBUG.send_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod types: type_cmd \'{0}\''.format(type_cmd))

        result = self.command_backend(file[0], type_cmd, map_file, file[0], fcontent)

        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod types: type_output\n{0}'.format(pprint.pformat(result)))

        for tyline in result:
            line = tyline.split(maxsplit=4)
            type_output.append({'note': {'expr': '',
                                         'type': line[4].replace('"', '')},
                                'source': {},
                                'region': {'from': {'line': int(line[0]),
                                                    'column': int(line[1])},
                                           'to': {'line': int(line[2]),
                                                  'column': int(line[3])}},
                                'level': None})

        return self.dispatch_callbacks(type_output, None, **backend_args)

    autofixes = Backend.default_method_implementation()
    refactor = Backend.default_method_implementation()
    rename = Backend.default_method_implementation()

    def langs(self, project_name, **backend_args):
        backend = self.project_backends.get(project_name)
        langs = backend.command_backend('lang') if backend is not None else []

        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod langs: resp =\n{0}'.format(langs))

        return self.dispatch_callbacks(langs, None, **backend_args)

    def flags(self, project_name, **backend_args):
        backend = self.project_backends.get(project_name)
        flags = backend.command_backend('flag') if backend is not None else []
        if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
            print('ghc-mod flags: resp =\n{0}'.format(flags))

        return self.dispatch_callbacks(flags, None, **backend_args)

    ghc_eval = Backend.default_method_implementation()
    ghc_type = Backend.default_method_implementation()

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
        backend_info = self.file_to_project.get(filename)
        return backend_info[1] if backend_info else None

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

        return []

    def translate_regex_output(self, cmd, files, contents, regex, xlat_func):
        retval = []
        for file in files:
            project_dir = self.get_project_dir(file)
            map_file = contents is not None and file in contents
            fcontent = contents[file] if map_file else None

            resp = self.command_backend(file, cmd + ' ' + file, map_file, file, fcontent)

            if Settings.COMPONENT_DEBUG.recv_messages or Settings.COMPONENT_DEBUG.all_messages:
                print('ghc-mod: {0}: map_file {1}, resp =\n{2}'.format(cmd, map_file, pprint.pformat(resp)))

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

    def collect_completions(self, backend, modinfo, lookup):
        if Settings.COMPONENT_DEBUG.completions:
            print('ghc-mod collect_completions for {0}'.format(modinfo))

        modname, is_qualified, qualname = modinfo
        mod_imported = [symbols.Import(modname, is_qualified, qualname)]
        syms = backend.command_backend('browse -d -o ' + modname) if backend is not None else []
        Logging.log('ghc-mod collect_completions: syms {0}'.format(syms), Logging.LOG_DEBUG)
        return [self.parse_syminfo(sym, mod_imported) for sym in syms if sym.startswith(lookup)]


    def parse_syminfo(self, syminfo, mod_imported):
        '''Convert a ghc-mod 'browse' result into the expected symbol types
        '''
        name, declinfo = self.get_name_decl(syminfo)

        # The name is an operator name '(++)' -- remove the parens. Does not remove them if name is unit ('()').
        if name[0] == '(' and name[-1] == ')' and len(name) > 2:
            name = name[1:-1]

        decl = None
        if declinfo.startswith('class '):
            ctx, args = self.split_context_args(name, declinfo[len('class '):])
            decl = symbols.Class(name, ctx, args, imported=mod_imported)
        elif declinfo.startswith('data '):
            ctx, args = self.split_context_args(name, declinfo[len('data '):])
            decl = symbols.Data(name, ctx, args, imported=mod_imported)
        elif declinfo.startswith('newtype '):
            ctx, args = self.split_context_args(name, declinfo[len('newtype '):])
            decl = symbols.Newtype(name, ctx, args, imported=mod_imported)
        elif declinfo.startswith('type '):
            ctx, args = self.split_context_args(name, declinfo[len('type '):])
            decl = symbols.Type(name, ctx, args, imported=mod_imported)
        else:
            # Default to function
            decl = symbols.Function(name, declinfo, imported=mod_imported)

        return decl


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
        if debug_any():
            print('Starting \'ghc-mod\' for project {0}'.format(project))

        self.ghcmod = None
        self.action_lock = None
        self.stderr_drain = None
        self.cmd = []
        self.diag_prefix = 'ghc-mod: project ' + project

        win = sublime.active_window()
        msg = 'Error and diagnostic output from ' + self.diag_prefix
        banner = '~' * len(msg)
        self.output_panel = Common.output_panel(win, panel_name=self.diag_prefix,
                                                text='\n'.join([banner, msg, banner, '', '']))
        panel_settings = self.output_panel.settings()
        panel_settings.set("auto_indent", False)
        panel_settings.set("smart_indent", False)

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

        if debug_any():
            print('ghc-mod command: {0}'.format(self.cmd))

        self.ghcmod = ProcHelper.ProcHelper(self.cmd, cwd=project_dir)
        if self.ghcmod.process is not None:
            self.ghcmod.process.stdin = io.TextIOWrapper(self.ghcmod.process.stdin, 'utf-8')
            self.ghcmod.process.stdout = io.TextIOWrapper(self.ghcmod.process.stdout, 'utf-8')
            self.action_lock = threading.Lock()
            self.stderr_drain = OutputCollector.DescriptorDrain(self.diag_prefix, self.ghcmod.process.stderr)
            self.stderr_drain.start()
        else:
            Logging.log('Did not start ghc-mod ({0}) successfully.'.format(self.diag_prefix))

    def shutdown(self):
        if self.ghcmod is not None:
            try:
                print('', file=self.ghcmod.process.stdin, flush=True)
            except OSError:
                pass
        if self.stderr_drain and self.stderr_drain.is_alive():
            self.stderr_drain.stop()
            self.stderr_drain.join()

        self.ghcmod = None
        self.action_lock = None
        self.stderr_drain = None

    def read_response(self):
        resp_stdout = []
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
                        # Just log the error output, just like the Emacs version
                        self.output_panel.run_command('insert', {'characters': resp + '\n'})
                    elif prefix == 'NG ':
                        sys.stdout.write('{0} malformed command or error response: {1}'.format(self.diag_prefix, resp))
                        got_reply = True
                    else:
                        sys.stdout.write('Unexpected reply from ghc-mod client: ' + resp)
                        got_reply = True
        except OSError:
            self.shutdown()

        return resp_stdout

    def command_backend(self, cmd, do_map=False, file=None, contents=None):
        if not self.action_lock:
            return ([], ['No ghc-mod backend for {0}'.format(self.diag_prefix)])

        with self.action_lock:
            try:
                if do_map:
                    if debug_send():
                        print('{0}.command_backend: mapping file {1}'.format(type(self).__name__, file))
                    print('map-file ' + file, file=self.ghcmod.process.stdin)
                    self.ghcmod.process.stdin.write(contents)
                    self.ghcmod.process.stdin.write('\n' + chr(4) + '\n')
                    self.ghcmod.process.stdin.flush()
                    resp = self.read_response()
                    if debug_recv():
                        print('{0}.command_backend: map-file received {1}'.format(type(self).__name__, resp))
                try:
                    if debug_send():
                        print('{0}.command_backend: sending {1}'.format(type(self).__name__, cmd))
                    print(cmd, file=self.ghcmod.process.stdin, flush=True)
                    resp = self.read_response()
                    if debug_recv():
                        print('{0}.command_backend: received {1}'.format(type(self).__name__, resp))
                    return resp
                except OSError:
                    self.shutdown()
                    return []
                finally:
                    if do_map:
                        if debug_send():
                            print('{0}.command_backend: mapping file {1}'.format(type(self).__name__, file))
                        print('unmap-file ' + file, file=self.ghcmod.process.stdin, flush=True)
                        unmap_resp = self.read_response()
                        if debug_recv():
                            print('{0}.command_backend: unmap-file received {1}'.format(type(self).__name__, unmap_resp))
            except OSError:
                self.shutdown()
                return []
