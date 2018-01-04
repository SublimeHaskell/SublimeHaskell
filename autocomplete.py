# -*- coding: UTF-8 -*-

"""SublimeHaskell autocompletion support."""

import re

import SublimeHaskell.internals.atomics as Atomics
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common


# Checks if we are in an import statement.
IMPORT_RE = re.compile(r'.*import(\s+qualified)?\s+')
IMPORT_RE_PREFIX = re.compile(r'^\s*import(\s+qualified)?\s+([\w\d\.]*)$')
IMPORT_QUALIFIED_POSSIBLE_RE = re.compile(r'.*import\s+(?P<qualifiedprefix>\S*)$')

# Checks if a word contains only alphanums, -, and _, and dot
NO_SPECIAL_CHARS_RE = re.compile(r'^(\w|[\-\.])*$')

# Export module
EXPORT_MODULE_RE = re.compile(r'\bmodule\s+[\w\d\.]*$')


def sort_completions(comps):
    comps.sort(key=lambda k: k[0])


def sorted_completions(comps):
    return sorted(set(comps))  # unique


def make_completions(suggestions):
    # return sorted_completions([s.suggest() for s in suggestions or []])
    return sorted([s.suggest() for s in suggestions or []])


def make_locations(comps):
    return sorted([[s.brief(), s.get_source_location()] for s in comps if s.has_source_location()],
                  key=lambda k: k[0])


class CompletionCache(Atomics.LockedObject):
    def __init__(self):
        super().__init__()
        self.files = {}
        self.cabal = []
        self.sources = []
        self.global_comps = []
        self.source_locs = None

    def __enter__(self):
        super().__enter__()
        return self

    # Technically, useless super() delegation. Uncomment if more is needed.
    # def __exit__(self, otype, value, traceback):
    #     super().__exit__(otype, value, traceback)

    def set_files(self, filename, comps):
        self.files[filename] = comps

    def set_cabal(self, comps):
        self.cabal = comps
        self.global_comps = sorted_completions(self.cabal + self.sources)

    def set_sources(self, comps):
        self.sources = comps
        self.global_comps = sorted_completions(self.cabal + self.sources)

    def set_locs(self, locs):
        self.source_locs = locs

    def global_completions(self):
        return self.global_comps


# Autocompletion data
class AutoCompleter(object):
    '''All of the logic (or lack thereof) behind Haskell symbol completions.
    '''
    def __init__(self):
        self.language_pragmas = []
        self.flags_pragmas = []

        # cabal name => set of modules, where cabal name is 'cabal' for cabal or sandbox path
        # for cabal-devs
        self.module_completions = Atomics.AtomicDuck()

        # keywords
        self.keywords = ['do', 'case', 'of', 'let', 'in', 'data', 'instance', 'type', 'newtype', 'where',
                         'deriving', 'import', 'module']

        self.current_filename = None

        # filename ⇒ preloaded completions + None ⇒ all completions
        self.cache = CompletionCache()

    def keyword_completions(self, query):
        return [(k + '\tkeyword', k) for k in self.keywords if k.startswith(query)] if isinstance(query, ''.__class__) else []

    def generate_completions_cache(self, project_name, file_name, contents=None):
        def log_result(result):
            retval = result or []
            if Settings.COMPONENT_DEBUG.completions:
                print('completions: {0}'.format(len(retval)))
            return retval

        comps = []
        update_cabal = False
        update_sources = False
        with self.cache as cache_:
            if file_name in cache_.files:
                del cache_.files[file_name]
            else:
                update_cabal = not cache_.cabal
                update_sources = not cache_.sources

        ## Not sure what these were supposed to do -- the actual methods are no-ops.
        if update_cabal:
            self.update_cabal_completions()
        if update_sources:
            self.update_sources_completions()

        with self.cache as cache_:
            comps = cache_.global_completions()

        import_names = []

        if file_name:
            if Settings.COMPONENT_DEBUG.completions:
                print('preparing completions for {0} ({1})'.format(project_name, file_name))

            backend = BackendManager.active_backend()
            comps = make_completions(backend.complete(Common.QualifiedSymbol(''), file_name, contents=contents))
            current_module = Utils.head_of(backend.module(project_name, file_name))

            if Settings.COMPONENT_DEBUG.completions:
                print('current_module {0}'.format(current_module))

            if current_module:
                # Get import names
                #
                # Note, that if module imported with 'as', then it can be used only with its synonym
                # instead of full name
                import_names.extend([('{0}\tmodule {1}'.format(i.import_as, i.module), i.import_as)
                                     for i in current_module.imports if i.import_as])
                import_names.extend([('{0}\tmodule'.format(i.module), i.module)
                                     for i in current_module.imports if not i.import_as])

                comps.extend(import_names)
                sort_completions(comps)

            with self.cache as cache_:
                cache_.files[file_name] = comps
                return log_result(cache_.files[file_name])
        else:
            return log_result(comps)

    def drop_completions_async(self, file_name=None):
        Logging.log('drop prepared completions', Logging.LOG_DEBUG)
        with self.cache as cache_:
            if file_name is None:
                cache_.files.clear()
            elif file_name in cache_.files:
                del cache_.files[file_name]

    def update_cabal_completions(self):
        pass

    def update_sources_completions(self):
        pass

    def get_completions(self, view, locations):
        "Get all the completions related to the current file."

        current_file_name = view.file_name()
        if not current_file_name:
            return []

        if Settings.COMPONENT_DEBUG.completions:
            print('AutoCompleter.get_completions.')

        self.current_filename = current_file_name
        _, project_name = Common.locate_cabal_project_from_view(view)

        line_contents = Common.get_line_contents(view, locations[0])
        qsymbol = Common.get_qualified_symbol(line_contents)
        qualified_prefix = qsymbol.qualified_name()

        if Settings.COMPONENT_DEBUG.completions:
            print('qsymbol {0}'.format(qsymbol))
            print('current_file_name {0} qualified_prefix {1}'.format(current_file_name, qualified_prefix))

        view_settings = view.settings()
        wide = view_settings.get('subhask_wide_completion')
        backend = BackendManager.active_backend()

        suggestions = []
        completions = []
        if qsymbol.module:
            if qsymbol.is_import_list:
                current_module = Utils.head_of(backend.module(project_name, current_file_name))
                if current_module and current_module.location.project:
                    # Search for declarations of qsymbol.module within current project
                    q_module = Utils.head_of(backend.scope_modules(project_name, current_file_name, lookup=qsymbol.module,
                                                                   search_type='exact'))
                    if q_module is not None:
                        if q_module.by_source():
                            proj_module = backend.resolve(file=q_module.location.filename, exports=True)
                            if proj_module:
                                suggestions = proj_module.declarations.values()
                        elif q_module.by_cabal():
                            cabal_module = Utils.head_of(backend.module(project_name, lookup=q_module.name, search_type='exact',
                                                                        package=q_module.location.package.name))
                            if cabal_module:
                                suggestions = cabal_module.declarations.values()
            else:
                if Settings.COMPONENT_DEBUG.completions:
                    print('completions: querying module-specific completions')
                suggestions = backend.complete(qsymbol, current_file_name, wide=wide)

            completions = make_completions(suggestions)
        else:
            with self.cache as cache_:
                if wide:
                    if Settings.COMPONENT_DEBUG.completions:
                        print('completions: returning global completions')
                    completions += cache_.global_completions()
                else:
                    if Settings.COMPONENT_DEBUG.completions:
                        print('completions: returning file-specific completions')
                    completions += cache_.files.get(current_file_name, cache_.global_completions())

        completions += self.keyword_completions(qsymbol.name)
        sort_completions(completions)
        return completions

    def completions_for_module(self, project_name, module, filename):
        """
        Returns completions for module
        """
        retval = []
        backend = BackendManager.active_backend()
        if module:
            mods = backend.scope_modules(project_name, filename, lookup=module, search_type='exact') if filename else []

            mod_file = mods[0].location.filename if mods and mods[0].by_source() else None
            cache_db = mods[0].location.db if mods and mods[0].by_cabal() else None
            package = mods[0].location.package.name if mods and mods[0].by_cabal() else None

            mod_decls = Utils.head_of(backend.module(project_name, lookup=module, search_type='exact', file=mod_file,
                                                     symdb=cache_db, package=package))
            retval = make_completions(mod_decls.declarations.values()) if mod_decls else []

        return retval

    def get_import_completions(self, project_name, filename, _locations, line_contents):

        # Autocompletion for import statements
        if Settings.PLUGIN.auto_complete_imports:
            self.current_filename = filename

            match_import_list = Common.IMPORT_SYMBOL_RE.search(line_contents)
            if match_import_list:
                module_name = match_import_list.group('module')
                return self.completions_for_module(project_name, module_name, self.current_filename)

            match_import = IMPORT_RE_PREFIX.match(line_contents)
            if match_import:
                (_, prefix) = match_import.groups()
                import_completions = list(set(self.get_module_completions_for(project_name, prefix)))
                sort_completions(import_completions)

                # Right after "import "? Propose "qualified" as well!
                qualified_match = IMPORT_QUALIFIED_POSSIBLE_RE.match(line_contents)
                if qualified_match:
                    qualified_prefix = qualified_match.group('qualifiedprefix')
                    if qualified_prefix == "" or "qualified".startswith(qualified_prefix):
                        import_completions.insert(0, (u"qualified", "qualified "))

                return import_completions

        return []

    def get_lang_completions(self, project_name):
        retval = []
        if Settings.PLUGIN.auto_complete_language_pragmas:
            retval = [[c, c] for c in BackendManager.active_backend().langs(project_name)]
            sort_completions(retval)

        return retval

    def get_flag_completions(self, project_name):
        retval = []
        if Settings.PLUGIN.auto_complete_language_pragmas:
            retval = [[c, c] for c in BackendManager.active_backend().flags(project_name)]
            sort_completions(retval)

        return retval

    def get_module_completions_for(self, project_name, qualified_prefix, modules=None, current_dir=None):
        def module_next_name(mname):
            """
            Returns next name for prefix
            pref = Control.Con, mname = Control.Concurrent.MVar, result = Concurrent.MVar
            """
            suffix = mname.split('.')[(len(qualified_prefix.split('.')) - 1):]
            # Sublime replaces full module name with suffix, if it contains no dots?
            return suffix[0]

        module_list = modules if modules else self.get_current_module_completions(project_name, current_dir)
        return list(set((module_next_name(m) + '\tmodule', module_next_name(m))
                        for m in module_list if m.startswith(qualified_prefix)))

    def get_current_module_completions(self, project_name, current_dir):
        """
        Get modules, that are in scope of file/project
        In case of file we just return 'scope modules' result
        In case of dir we look for a related project or sandbox:
            project - get dependent modules
            sandbox - get sandbox modules
        """
        backend = BackendManager.active_backend()
        if self.current_filename:
            return set([m.name for m in backend.scope_modules(project_name, self.current_filename)])
        elif current_dir:
            proj = backend.project(path=current_dir)
            if proj and 'path' in proj:
                return set([m.name for m in backend.list_modules(deps=proj['path'])])
            sbox = backend.sandbox(path=current_dir)
            if sbox and isinstance(sbox, dict) and 'sandbox' in sbox:
                sbox = sbox.get('sandbox')
            if sbox:
                mods = backend.list_modules(sandbox=sbox) or []
                return set([m.name for m in mods])
        else:
            mods = backend.list_modules(cabal=True) or []
            return set([m.name for m in mods])
