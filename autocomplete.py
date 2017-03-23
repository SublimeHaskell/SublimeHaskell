# -*- coding: UTF-8 -*-
# pyline: disable=todo

"""SublimeHaskell autocompletion support."""

import re
import sublime
import sublime_plugin
import time

if int(sublime.version()) < 3000:
    import sublime_haskell_common as Common
    import internals.logging as Logging
    import internals.locked_object as LockedObject
    import internals.settings as Settings
    import internals.utils as Utils
    from hdevtools import start_hdevtools, stop_hdevtools
    import ghci_backend as GHCIMod
    import hsdev
    from worker import run_async
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.logging as Logging
    import SublimeHaskell.internals.locked_object as LockedObject
    import SublimeHaskell.internals.settings as Settings
    import SublimeHaskell.internals.utils as Utils
    from SublimeHaskell.hdevtools import start_hdevtools, stop_hdevtools
    import SublimeHaskell.ghci_backend as GHCIMod
    import SublimeHaskell.hsdev as hsdev
    from SublimeHaskell.worker import run_async


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

    if Settings.get_setting_async('enable_hsdev'):
        return hsdev.client.langs()
    elif Settings.get_setting_async('enable_ghc_mod'):
        return GHCIMod.call_ghcmod_and_wait(['lang']).splitlines()

    return []


def get_flags_pragmas():

    if Settings.get_setting_async('enable_hsdev'):
        return hsdev.client.flags()
    elif Settings.get_setting_async('enable_ghc_mod'):
        return GHCIMod.call_ghcmod_and_wait(['flag']).splitlines()

    return []


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
class AutoCompletion(object):
    """Information for completion"""
    def __init__(self):
        self.language_pragmas = []
        self.flags_pragmas = []

        # cabal name => set of modules, where cabal name is 'cabal' for cabal or sandbox path
        # for cabal-devs
        self.module_completions = LockedObject.LockedObject({})

        # keywords
        # TODO: keywords can't appear anywhere, we can suggest in right places
        self.keyword_completions = list(map(
            lambda k: (k + '\tkeyword', k),
            ['do', 'case', 'of', 'let', 'in', 'data', 'instance', 'type', 'newtype', 'where',
             'deriving', 'import', 'module']))

        self.current_filename = None

        # filename ⇒ preloaded completions + None ⇒ all completions
        self.cache = LockedObject.LockedObject(CompletionCache())
        self.wide_completion = None

    def mark_wide_completion(self, view):
        self.wide_completion = view

    @hsdev.use_hsdev([])
    def get_completions_async(self, file_name=None):
        def log_result(r):
            Logging.log('completions: {0}'.format(len(r or [])), Logging.LOG_TRACE)
            return r or []
        none_comps = []
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
            none_comps = cache_.global_completions()

        import_names = []
        comps = none_comps

        if file_name is None:
            return log_result(none_comps)
        else:
            Logging.log('preparing completions for {0}'.format(file_name), Logging.LOG_DEBUG)
            current_module = Utils.head_of(hsdev.client_back.module(file=file_name))
            if current_module:
                comps = make_completions(
                    hsdev.client_back.complete('', file_name, timeout=None))

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
        if window:
            view = window.active_view()
            if view and Common.is_haskell_source(view):
                filename = view.file_name()
                if filename:
                    self.get_completions_async(filename)

    @hsdev.use_hsdev([])
    def get_completions(self, view, prefix, locations):
        "Get all the completions that related to the current file."

        current_file_name = view.file_name()

        if not current_file_name:
            return []

        self.current_filename = current_file_name
        line_contents = Common.get_line_contents(view, locations[0])
        qsymbol = Common.get_qualified_symbol(line_contents)
        qualified_prefix = qsymbol.qualified_name()

        suggestions = []

        wide = self.wide_completion == view
        if wide:  # Drop wide
            self.wide_completion = None

        if qsymbol.module:
            current_module = Utils.head_of(hsdev.client.module(file=current_file_name))
            current_project = None
            if current_module:
                current_project = current_module.location.project
                if qsymbol.is_import_list:
                    if current_project:
                        # Search for declarations of qsymbol.module within current project
                        q_module = head_of(hsdev.client.scope_modules(file=current_file_name,
                                                                      input=qsymbol.module,
                                                                      search_type='exact'))
                        if q_module.by_source():
                            proj_module = hsdev.client.resolve(file=q_module.location.filename,
                                                               exports=True)
                            if proj_module:
                                suggestions = proj_module.declarations.values()
                        elif q_module.by_cabal():
                            cabal_module = head_of(hsdev.client.module(q_module.name,
                                                                       search_type='exact',
                                                                       package=q_module.location.package.name))
                            if cabal_module:
                                suggestions = cabal_module.declarations.values()
                else:
                    suggestions = hsdev.client.complete(qualified_prefix, current_file_name,
                                                        wide=wide)
            return self.keyword_completions + make_completions(suggestions)
        else:
            with self.cache as cache_:
                if wide:
                    return self.keyword_completions + cache_.global_completions()
                else:
                    return self.keyword_completions + cache_.files.get(current_file_name,
                                                                       cache_.global_completions())

    @hsdev.use_hsdev([])
    def completions_for_module(self, module, filename=None):
        """
        Returns completions for module
        """
        if not module:
            return []
        ms = []
        if filename:
            ms = hsdev.client.scope_modules(filename, input=module, search_type='exact')
        m = head_of(hsdev.client.module(
            input=module,
            search_type='exact',
            file=ms[0].location.filename if ms and ms[0].by_source() else None,
            db=ms[0].location.db if ms and ms[0].by_cabal() else None,
            package=ms[0].location.package.name if ms and ms[0].by_cabal() else None))
        if not m:
            return []
        return make_completions(m.declarations.values())

    def completions_for(self, module_name, filename=None):
        """
        Returns completions for module
        """
        return self.completions_for_module(module_name, filename)

    @hsdev.use_hsdev([])
    def get_import_completions(self, view, prefix, locations):

        self.current_filename = view.file_name()
        line_contents = Common.get_line_contents(view, locations[0])

        # Autocompletion for import statements
        if Settings.get_setting('auto_complete_imports'):
            match_import_list = Common.IMPORT_SYMBOL_RE.search(line_contents)
            if match_import_list:
                module_name = match_import_list.group('module')
                import_list_completions = []

                import_list_completions.extend(self.completions_for(module_name, self.current_filename))

                return import_list_completions

            match_import = IMPORT_RE_PREFIX.match(line_contents)
            if match_import:
                (qualified, pref) = match_import.groups()
                import_completions = self.get_module_completions_for(pref)

                # Right after "import "? Propose "qualified" as well!
                qualified_match = IMPORT_QUALIFIED_POSSIBLE_RE.match(line_contents)
                if qualified_match:
                    qualified_prefix = qualified_match.group('qualifiedprefix')
                    if qualified_prefix == "" or "qualified".startswith(qualified_prefix):
                        import_completions.insert(0, (u"qualified", "qualified "))

                return list(set(import_completions))

        return []

    def get_special_completions(self, view, prefix, locations):

        # Contents of the current line up to the cursor
        line_contents = Common.get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if Settings.get_setting('auto_complete_language_pragmas'):
            # TODO handle multiple selections
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                if not self.language_pragmas:
                    self.language_pragmas = get_language_pragmas()
                return [(Utils.to_unicode(c),) * 2 for c in self.language_pragmas]
            match_options = OPTIONS_GHC_RE.match(line_contents)
            if match_options:
                if not self.flags_pragmas:
                    self.flags_pragmas = get_flags_pragmas()
                return [(Utils.to_unicode(c),) * 2 for c in self.flags_pragmas]

        return []

    @hsdev.use_hsdev([])
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

    @hsdev.use_hsdev([])
    def get_current_module_completions(self, current_dir=None):
        """
        Get modules, that are in scope of file/project
        In case of file we just return 'scope modules' result
        In case of dir we look for a related project or sandbox:
            project - get dependent modules
            sandbox - get sandbox modules
        """
        if self.current_filename:
            return set([m.name for m in hsdev.client.scope_modules(self.current_filename)])
        elif current_dir:
            proj = hsdev.client.project(path=current_dir)
            if proj and 'path' in proj:
                return set([m.name for m in hsdev.client.list_modules(deps=proj['path'])])
            sbox = hsdev.client.sandbox(path=current_dir)
            if sbox and type(sbox) == dict and 'sandbox' in sbox:
                sbox = sbox.get('sandbox')
            if sbox:
                mods = hsdev.client.list_modules(sandbox=sbox) or []
                return set([m.name for m in mods])
        else:
            mods = hsdev.client.list_modules(cabal=True) or []
            return set([m.name for m in mods])

autocompletion = AutoCompletion()


def can_complete_qualified_symbol(info):
    """
    Helper function, returns whether sublime_haskell_complete can run for (module, symbol, is_import_list)
    """
    if not info.module:
        return False

    if info.is_import_list:
        return info.module in autocompletion.get_current_module_completions()
    else:
        return list(filter(lambda m: m.startswith(info.module), autocompletion.get_current_module_completions())) != []


def update_completions_async(files=[], drop_all=False):
    if drop_all:
        run_async('drop all completions', autocompletion.drop_completions_async)
    else:
        for f in files:
            run_async('drop completions', autocompletion.drop_completions_async, f)
    run_async('init completions', autocompletion.init_completions_async)


class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    def __init__(self):
        self.project_file_name = None

    def on_query_completions(self, view, prefix, locations):
        if not Common.is_haskell_source(view):
            return []

        begin_time = time.clock()
        # Only suggest symbols if the current file is part of a Cabal project.

        completions = (autocompletion.get_import_completions(view, prefix, locations) +
                       autocompletion.get_special_completions(view, prefix, locations))

        # Export list
        if 'meta.declaration.exports.haskell' in view.scope_name(view.sel()[0].a):
            line_contents = Common.get_line_contents(view, locations[0])
            export_module = EXPORT_MODULE_RE.search(line_contents)
            if export_module:
                # qsymbol = Common.get_qualified_symbol_at_region(view, view.sel()[0])
                # TODO: Implement
                pass

        if not completions:
            completions = autocompletion.get_completions(view, prefix, locations)

        end_time = time.clock()
        Logging.log('time to get completions: {0} seconds'.format(end_time - begin_time), Logging.LOG_DEBUG)

        # Don't put completions with special characters (?, !, ==, etc.)
        # into completion because that wipes all default Sublime completions:
        # See http://www.sublimetext.com/forum/viewtopic.php?t=8659
        # TODO: work around this
        # comp = [c for c in completions if NO_SPECIAL_CHARS_RE.match(c[0].split('\t')[0])]
        # if Settings.get_setting('inhibit_completions') and len(comp) != 0:
        #     return (comp, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        # return comp

        if Settings.get_setting('inhibit_completions') and len(completions) != 0:
            return (completions, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        return completions

    def set_cabal_status(self, view):
        filename = view.file_name()
        if filename:
            (cabal_dir, project_name) = Common.get_cabal_project_dir_and_name_of_file(filename)
            if project_name:
                # TODO: Set some useful status instead of this
                view.set_status('sublime_haskell_cabal', '{0}: {1}'.format('cabal', project_name))

    def on_activated_async(self, view):
        if Common.is_haskell_source(view):
            filename = view.file_name()
            if filename:
                run_async('get completions for {0}'.format(filename), autocompletion.get_completions_async, filename)

    def on_new(self, view):
        hsdev.start_agent()

        self.set_cabal_status(view)
        if Common.is_inspected_source(view):
            filename = view.file_name()
            if filename:
                hsdev.agent.mark_file_dirty(filename)
                update_completions_async(drop_all=True)

    def on_load(self, view):
        hsdev.start_agent()

        self.set_cabal_status(view)
        if Common.is_inspected_source(view):
            filename = view.file_name()
            if filename:
                hsdev.agent.mark_file_dirty(filename)
                update_completions_async(drop_all=True)

    def on_activated(self, view):
        hsdev.start_agent()

        self.set_cabal_status(view)

        window = view.window()
        if window:
            if int(sublime.version()) < 3000:
                pass
            else:
                if not self.project_file_name:
                    self.project_file_name = window.project_file_name()
                if window.project_file_name() is not None and window.project_file_name() != self.project_file_name:
                    self.project_file_name = window.project_file_name()
                    Logging.log('project switched to {0}, reinspecting'.format(self.project_file_name))
                    if hsdev.agent_connected():
                        Logging.log('reinspect all', Logging.LOG_TRACE)
                        hsdev.client.remove_all()
                        hsdev.agent.start_inspect()
                        hsdev.agent.force_inspect()
                    else:
                        Common.show_status_message("inspector not connected", is_ok=False)

    def on_post_save(self, view):
        if Common.is_inspected_source(view):
            filename = view.file_name()
            if filename:
                hsdev.agent.mark_file_dirty(filename)
                update_completions_async(drop_all=True)


def plugin_loaded():
    # TODO: How to stop_hdevtools() in Sublime Text 2?
    start_hdevtools()


def plugin_unloaded():
    # Does this work properly on exit?
    stop_hdevtools()

if int(sublime.version()) < 3000:
    plugin_loaded()
