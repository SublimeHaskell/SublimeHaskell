# -*- coding: UTF-8 -*-

"""SublimeHaskell autocompletion support."""

import re

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils


# Checks if we are in a LANGUAGE pragma.
LANGUAGE_RE = re.compile(r'.*{-#\s+LANGUAGE.*')
# Checks if we are in OPTIONS_GHC pragma
OPTIONS_GHC_RE = re.compile(r'.*{-#\s+OPTIONS_GHC.*')

# Checks if we are in an import statement.
IMPORT_RE = re.compile(r'.*import(\s+qualified)?\s+')
IMPORT_RE_PREFIX = re.compile(r'^\s*import(\s+qualified)?\s+([\w\d\.]*)$')
IMPORT_QUALIFIED_POSSIBLE_RE = re.compile(r'.*import\s+(?P<qualifiedprefix>\S*)$')

# Checks if a word contains only alhanums, -, and _, and dot
NO_SPECIAL_CHARS_RE = re.compile(r'^(\w|[\-\.])*$')

# Export module
EXPORT_MODULE_RE = re.compile(r'\bmodule\s+[\w\d\.]*$')


# Gets available LANGUAGE options and import modules from ghc-mod
def get_language_pragmas():
    return BackendManager.active_backend().langs()
    # elif Settings.PLUGIN.enable_ghc_mod:
    #     return GHCIMod.call_ghcmod_and_wait(['lang']).splitlines()
    # else:
    #     return []


def get_flags_pragmas():
    return BackendManager.active_backend().flags()
    # elif Settings.PLUGIN.enable_ghc_mod:
    #     return GHCIMod.call_ghcmod_and_wait(['flag']).splitlines()
    # else:
    #     return []


def sort_completions(comps):
    comps.sort(key=lambda k: k[0])


def sorted_completions(comps):
    return list(set(comps))  # unique + sort


def make_completions(suggestions):
    return sorted_completions([s.suggest() for s in suggestions or []])


def make_locations(comps):
    return sorted([[s.brief(), s.get_source_location()] for s in comps if s.has_source_location()],
                  key=lambda k: k[0])


class CompletionCache(object):
    def __init__(self):
        self.files = {}
        self.cabal = []
        self.sources = []
        self.global_comps = []
        self.source_locs = None

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
    """Information for completion"""
    def __init__(self):
        self.language_pragmas = []
        self.flags_pragmas = []

        # cabal name => set of modules, where cabal name is 'cabal' for cabal or sandbox path
        # for cabal-devs
        self.module_completions = LockedObject.LockedObject({})

        # keywords
        self.keywords = ['do', 'case', 'of', 'let', 'in', 'data', 'instance', 'type', 'newtype', 'where',
                         'deriving', 'import', 'module']

        self.current_filename = None

        # filename ⇒ preloaded completions + None ⇒ all completions
        self.cache = LockedObject.LockedObject(CompletionCache())

    def keyword_completions(self, query):
        return [(k + '\tkeyword', k) for k in self.keywords if k.startswith(query)] if isinstance(query, str) else []

    def get_completions_async(self, file_name=None):
        def log_result(result):
            retval = result or []
            Logging.log('completions: {0}'.format(len(retval)), Logging.LOG_TRACE)
            return retval

        comps = []
        update_cabal = False
        update_sources = False
        with self.cache as cache_:
            if file_name in cache_.files:
                return log_result(cache_.files.get(file_name, []))
            else:
                update_cabal = not cache_.cabal
                update_sources = not cache_.sources
        if update_cabal:
            self.update_cabal_completions()
        if update_sources:
            self.update_sources_completions()
        with self.cache as cache_:
            comps = cache_.global_completions()

        import_names = []

        if file_name is None:
            return log_result(comps)
        else:
            Logging.log('preparing completions for {0}'.format(file_name), Logging.LOG_DEBUG)
            current_module = Utils.head_of(BackendManager.active_backend().module(file=file_name))

            if current_module:
                comps = make_completions(BackendManager.active_backend().complete('', file_name))

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

    def drop_completions_async(self, file_name=None):
        Logging.log('drop prepared completions')
        with self.cache as cache_:
            if file_name is None:
                cache_.files.clear()
            elif file_name in cache_.files:
                del cache_.files[file_name]

    def update_cabal_completions(self):
        pass

    def update_sources_completions(self):
        pass

    def init_completions_async(self):
        window = sublime.active_window()
        view = window.active_view() if window else None
        if view and Common.is_haskell_source(view):
            filename = view.file_name()
            if filename:
                self.get_completions_async(filename)

    def get_completions(self, view, locations):
        "Get all the completions related to the current file."

        current_file_name = view.file_name()
        if not current_file_name:
            return []

        Logging.log('AutoCompleter.get_completons.', Logging.LOG_DEBUG)

        self.current_filename = current_file_name

        backend = BackendManager.active_backend()
        line_contents = Common.get_line_contents(view, locations[0])
        qsymbol = Common.get_qualified_symbol(line_contents)
        qualified_prefix = qsymbol.qualified_name()

        Logging.log('qsymbol {0}'.format(qsymbol), Logging.LOG_DEBUG)
        Logging.log('current_file_name {0} qualified_prefix {1}'.format(current_file_name, qualified_prefix), Logging.LOG_DEBUG)

        view_settings = view.settings()
        wide = view_settings.get('subhask_wide_completion')

        suggestions = []
        completions = []
        if qsymbol.module:
            if qsymbol.is_import_list:
                current_module = Utils.head_of(backend.module(file=current_file_name))
                if current_module and current_module.location.project:
                    # Search for declarations of qsymbol.module within current project
                    q_module = Utils.head_of(backend.scope_modules(file=current_file_name,
                                                                   lookup=qsymbol.module,
                                                                   search_type='exact'))
                    if q_module is not None:
                        if q_module.by_source():
                            proj_module = backend.resolve(file=q_module.location.filename, exports=True)
                            if proj_module:
                                suggestions = proj_module.declarations.values()
                        elif q_module.by_cabal():
                            cabal_module = Utils.head_of(backend.module(lookup=q_module.name,
                                                                        search_type='exact',
                                                                        package=q_module.location.package.name))
                            if cabal_module:
                                suggestions = cabal_module.declarations.values()
            else:
                suggestions = backend.complete(qualified_prefix, current_file_name, wide=wide)

            completions = make_completions(suggestions)
        else:
            with self.cache as cache_:
                if wide:
                    completions = cache_.global_completions()
                else:
                    completions = cache_.files.get(current_file_name, cache_.global_completions())

        return completions + self.keyword_completions(qsymbol.name)

    def completions_for_module(self, module, filename=None):
        """
        Returns completions for module
        """
        retval = []
        backend = BackendManager.active_backend()
        if module:
            mods = backend.scope_modules(filename, lookup=module, search_type='exact') if filename else []

            mod_file = mods[0].location.filename if mods and mods[0].by_source() else None
            cache_db = mods[0].location.db if mods and mods[0].by_cabal() else None
            package = mods[0].location.package.name if mods and mods[0].by_cabal() else None

            mod_decls = Utils.head_of(backend.module(lookup=module, search_type='exact', file=mod_file, symdb=cache_db,
                                                     package=package))
            retval = make_completions(mod_decls.declarations.values()) if mod_decls else []

        return retval

    def completions_for(self, module_name, filename=None):
        """
        Returns completions for module
        """
        return self.completions_for_module(module_name, filename)

    def get_import_completions(self, view, _locations, line_contents):

        # Autocompletion for import statements
        if Settings.PLUGIN.auto_complete_imports:
            self.current_filename = view.file_name()

            match_import_list = Common.IMPORT_SYMBOL_RE.search(line_contents)
            if match_import_list:
                module_name = match_import_list.group('module')
                import_list_completions = []

                import_list_completions.extend(self.completions_for(module_name, self.current_filename))

                return import_list_completions

            match_import = IMPORT_RE_PREFIX.match(line_contents)
            if match_import:
                (_, pref) = match_import.groups()
                import_completions = self.get_module_completions_for(pref)

                # Right after "import "? Propose "qualified" as well!
                qualified_match = IMPORT_QUALIFIED_POSSIBLE_RE.match(line_contents)
                if qualified_match:
                    qualified_prefix = qualified_match.group('qualifiedprefix')
                    if qualified_prefix == "" or "qualified".startswith(qualified_prefix):
                        import_completions.insert(0, (u"qualified", "qualified "))

                return list(set(import_completions))

        return []

    def get_special_completions(self, line_contents):
        # Autocompletion for LANGUAGE pragmas
        if Settings.PLUGIN.auto_complete_language_pragmas:
            # TODO handle multiple selections
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                if not self.language_pragmas:
                    self.language_pragmas = get_language_pragmas()
                return [(c,) * 2 for c in self.language_pragmas]
            match_options = OPTIONS_GHC_RE.match(line_contents)
            if match_options:
                if not self.flags_pragmas:
                    self.flags_pragmas = get_flags_pragmas()
                return [(c,) * 2 for c in self.flags_pragmas]

        return []

    def get_module_completions_for(self, qualified_prefix, modules=None, current_dir=None):
        def module_next_name(mname):
            """
            Returns next name for prefix
            pref = Control.Con, mname = Control.Concurrent.MVar, result = Concurrent.MVar
            """
            suffix = mname.split('.')[(len(qualified_prefix.split('.')) - 1):]
            # Sublime replaces full module name with suffix, if it contains no dots?
            return suffix[0]

        module_list = modules if modules else self.get_current_module_completions(current_dir=current_dir)
        return list(set((module_next_name(m) + '\tmodule', module_next_name(m))
                        for m in module_list if m.startswith(qualified_prefix)))

    def get_current_module_completions(self, current_dir=None):
        """
        Get modules, that are in scope of file/project
        In case of file we just return 'scope modules' result
        In case of dir we look for a related project or sandbox:
            project - get dependent modules
            sandbox - get sandbox modules
        """
        backend = BackendManager.active_backend()
        if self.current_filename:
            return set([m.name for m in backend.scope_modules(self.current_filename)])
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
