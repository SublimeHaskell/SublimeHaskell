import json
import os
import re
import sublime
import sublime_plugin
import threading
import time

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import symbols
    import cache
    import util
    import hdocs
    from ghci import ghci_info
    from haskell_docs import haskell_docs
    from hdevtools import start_hdevtools, stop_hdevtools
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.symbols as symbols
    import SublimeHaskell.cache as cache
    import SublimeHaskell.util as util
    import SublimeHaskell.hdocs as hdocs
    from SublimeHaskell.ghci import ghci_info
    from SublimeHaskell.haskell_docs import haskell_docs
    from SublimeHaskell.hdevtools import start_hdevtools, stop_hdevtools


# If true, files that have not changed will not be re-inspected.
CHECK_MTIME = True

MODULE_INSPECTOR_SOURCE_PATH = None
MODULE_INSPECTOR_EXE_PATH = None
MODULE_INSPECTOR_OBJ_DIR = None
CABAL_INSPECTOR_SOURCE_PATH = None
CABAL_INSPECTOR_EXE_PATH = None
CABAL_INSPECTOR_OBJ_DIR = None

# ModuleInspector output
MODULE_INSPECTOR_RE = re.compile(r'ModuleInfo:(?P<result>.+)')

# The agent sleeps this long between inspections.
AGENT_SLEEP_TIMEOUT = 60.0

# Checks if we are in a LANGUAGE pragma.
LANGUAGE_RE = re.compile(r'.*{-#\s+LANGUAGE.*')

# Checks if we are in an import statement.
IMPORT_RE = re.compile(r'.*import(\s+qualified)?\s+')
IMPORT_RE_PREFIX = re.compile(r'^\s*import(\s+qualified)?\s+(.*)$')
IMPORT_QUALIFIED_POSSIBLE_RE = re.compile(r'.*import\s+(?P<qualifiedprefix>\S*)$')

# Checks if a word contains only alhanums, -, and _, and dot
NO_SPECIAL_CHARS_RE = re.compile(r'^(\w|[\-\.])*$')

# Get symbol qualified prefix and its name
SYMBOL_RE = re.compile(r'((?P<module>\w+(\.\w+)*)\.)?(?P<identifier>\w*)$')
# Get symbol module scope and its name within import statement
IMPORT_SYMBOL_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>\w+(\.\w+)*)(\s+as\s+(?P<as>\w+))?\s*\(.*?(?P<identifier>\w*)$')

def get_line_contents(view, location):
    """
    Returns contents of line at the given location.
    """
    return view.substr(sublime.Region(view.line(location).a, location))

def get_line_contents_before_region(view, region):
    """
    Returns contents of line before the given region (including it).
    """
    return view.substr(sublime.Region(view.line(region).a, region.b))

def get_qualified_name(s):
    """
    'bla bla bla Data.List.fo' -> ('Data.List', 'Data.List.fo')
    """
    if len(s) == 0:
        return ('', '')
    quals = s.split()[-1].split('.')
    filtered = map(lambda s: list(filter(lambda c: c.isalpha() or c.isdigit() or c == '_', s)), quals)
    return ('.'.join(filtered[0:len(filtered) - 1]), '.'.join(filtered))

def get_qualified_symbol(line):
    """
    Get module context of symbol and symbol itself
    Returns (module, name, is_import_list), where module (or one of) can be None
    """
    res = IMPORT_SYMBOL_RE.search(line)
    if res:
        return (res.group('module'), res.group('identifier'), True)
    res = SYMBOL_RE.search(line)
    # res always match
    return (res.group('module'), res.group('identifier'), False)

def get_qualified_symbol_at_region(view, region):
    """
    Get module context of symbol and symbol itself for line before (and with) word on region
    Returns (module, name), where module (or one of) can be None
    """
    word_region = view.word(region)
    preline = get_line_contents_before_region(view, word_region)
    return get_qualified_symbol(preline)

# Autocompletion data
class AutoCompletion(object):
    """Information for completion"""
    def __init__(self):
        self.language_completions = []
        # cabal name => set of modules, where cabal name is 'cabal' for cabal or sandbox path for cabal-devs
        self.module_completions = LockedObject({})

        # Currently used projects
        # name => project where project is:
        #   dir - project dir
        #   cabal - cabal file
        #   executables - list of executables where executable is
        #     name - name of executable
        self.projects = LockedObject({})

        # Storage of information
        self.database = symbols.Database()

        # keywords
        # TODO: keywords can't appear anywhere, we can suggest in right places
        self.keyword_completions = map(
            lambda k: (k + '\t(keyword)', k),
            ['case', 'data', 'instance', 'type', 'where', 'deriving', 'import', 'module'])

        self.current_filename = None

    def clear_inspected(self):
        # self.info = {}
        # self.std_info = {}
        self.projects.object = {}
        self.database = symbols.Database()

    def unalias_module_name(self, view, alias):
        "Get module names by alias"
        current_file_name = view.file_name()
        with self.database.files as files:
            if current_file_name in files:
                return files[current_file_name].unalias(alias)
        return []

    def get_completions(self, view, prefix, locations):
        "Get all the completions that apply to the current file."

        current_file_name = view.file_name()
        self.current_filename = current_file_name

        # Contents of the line under the first cursor
        line_contents = get_line_contents(view, locations[0])

        # If the current line is an import line, gives us (My.Module, ident)
        (qualified_module, symbol_name, is_import_list) = get_qualified_symbol(line_contents)
        qualified_prefix = '{0}.{1}'.format(qualified_module, symbol_name) if qualified_module else symbol_name

        # The list of completions we're going to assemble
        completions = []

        # Complete with modules too
        if qualified_module and not is_import_list:
            completions.extend(self.get_module_completions_for(qualified_prefix))

        moduleImports = []

        cur_info = None
        with self.database.files as files:
            if current_file_name in files:
                cur_info = files[current_file_name]

        if cur_info:
            if qualified_module:
                # If symbol is qualified, use completions from module specified
                moduleImports.append(qualified_module)
                moduleImports.extend([i.module for i in cur_info.imports.values() if i.import_as == qualified_module])
            else:
                # Otherwise, use completions from all importred unqualified modules and from this module
                moduleImports.append('Prelude')
                moduleImports.extend([i.module for i in cur_info.imports.values() if not i.is_qualified])
                # Add this module as well
                completions.extend(self.completions_for_module(cur_info, current_file_name))
                # Add keyword completions and module completions
                completions.extend(self.keyword_completions)
                completions.extend(self.get_module_completions_for(qualified_prefix, [i.module for i in cur_info.imports.values()]))

        for mi in set(moduleImports):
            completions.extend(self.completions_for(mi, current_file_name))

        return list(set(completions))

    def completions_for_module(self, module, filename = None):
        """
        Returns completions for module
        """
        if not module:
            return []
        return map(lambda d: d.suggest(), module.declarations.values())

    def completions_for(self, module_name, filename = None):
        """
        Returns completions for module
        """
        with self.database.modules as modules:
            if module_name not in modules:
                return []
            # TODO: Show all possible completions?
            return self.completions_for_module(symbols.get_visible_module(modules[module_name], filename), filename)

    def get_import_completions(self, view, prefix, locations):

        self.current_filename = view.file_name()

        # Contents of the current line up to the cursor
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if get_setting('auto_complete_language_pragmas'):
            # TODO handle multiple selections
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                return [(unicode(c),) * 2 for c in self.language_completions]

        # Autocompletion for import statements
        if get_setting('auto_complete_imports'):
            match_import_list = IMPORT_SYMBOL_RE.search(line_contents)
            if match_import_list:
                module_name = match_import_list.group('module')
                import_list_completions = []

                import_list_completions.extend(self.completions_for(module_name, self.current_filename))

                return import_list_completions

            match_import = IMPORT_RE_PREFIX.match(line_contents)
            if match_import:
                (qualified, pref) = match_import.groups()
                import_completions = self.get_module_completions_for(pref)

                # Right after "import " Propose "qualified" as well!
                qualified_match = IMPORT_QUALIFIED_POSSIBLE_RE.match(line_contents)
                if qualified_match:
                    qualified_prefix = qualified_match.group('qualifiedprefix')
                    if qualified_prefix == "" or "qualified".startswith(qualified_prefix):
                        import_completions.insert(0, (u"qualified", "qualified "))

                return list(set(import_completions))

        return None

    def get_module_completions_for(self, qualified_prefix, modules = None):
        def module_next_name(mname):
            """
            Returns next name for prefix
            pref = Control.Con, mname = Control.Concurrent.MVar, result = Concurrent.MVar
            """
            suffix = mname.split('.')[(len(qualified_prefix.split('.')) - 1):]
            # Sublime replaces full module name with suffix, if it contains no dots?
            return suffix[0]

        module_list = modules if modules else self.get_current_module_completions()
        return list(set((module_next_name(m) + '\t(module)', module_next_name(m)) for m in module_list if m.startswith(qualified_prefix)))

    def get_current_module_completions(self):
        completions = []

        cabal = current_cabal()

        with self.database.get_cabal_modules() as cabal_modules:
            completions.extend(list(cabal_modules.keys()))

        if self.current_filename:
            (project_path, _) = get_cabal_project_dir_and_name_of_file(self.current_filename)
            if project_path:
                completions.extend([m.name for m in self.database.get_project_modules(project_path).values()])

        with self.module_completions as module_completions:
            if cabal in module_completions:
                completions.extend(module_completions[cabal])

        return set(completions)


autocompletion = AutoCompletion()



def can_complete_qualified_symbol(info):
    """
    Helper function, returns whether sublime_haskell_complete can run for (module, symbol, is_import_list)
    """
    (module_name, symbol_name, is_import_list) = info
    if not module_name:
        return False

    if is_import_list:
        return module_name in autocompletion.get_current_module_completions()
    else:
        return list(filter(lambda m: m.startswith(module_name), autocompletion.get_current_module_completions())) != []

class SublimeHaskellComplete(sublime_plugin.TextCommand):
    """ Shows autocompletion popup """
    def run(self, edit, characters):
        for region in self.view.sel():
            self.view.insert(edit, region.end(), characters)

        if can_complete_qualified_symbol(get_qualified_symbol_at_region(self.view, self.view.sel()[0])):
            self.view.run_command("hide_auto_complete")
            sublime.set_timeout(self.do_complete, 1)

    def do_complete(self):
        self.view.run_command("auto_complete")

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)



class SublimeHaskellBrowseDeclarations(sublime_plugin.WindowCommand):
    """
    Show all available declarations from current cabal and opened projects
    """
    def run(self):
        self.decls = []
        self.declarations = []

        # (module, ident) => symbols.Declaration
        decls = {}

        with autocompletion.database.files as files:
            for m in files.values():
                for decl in m.declarations.values():
                    decls[(m.name, decl.name)] = decl

        for decl in decls.values():
            self.decls.append(decl)
            self.declarations.append(decl.module.name + ': ' + decl.brief())

        self.window.show_quick_panel(self.declarations, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        view = self.window.active_view()
        if not view:
            return

        decl = self.decls[idx]

        view.run_command('sublime_haskell_symbol_info', {
            'filename': decl.location.filename,
            'decl': decl.name })

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)



class SublimeHaskellGoToAnyDeclaration(sublime_plugin.WindowCommand):
    def run(self):
        self.files = []
        self.declarations = []

        with autocompletion.database.files as files:
            for f, m in files.items():
                for decl in m.declarations.values():
                    self.files.append([f, str(decl.location.line), str(decl.location.column)])
                    self.declarations.append([decl.brief(), '{0}:{1}:{2}'.format(decl.module.name, decl.location.line, decl.location.column)])

        self.window.show_quick_panel(self.declarations, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        self.window.open_file(':'.join(self.files[idx]), sublime.ENCODED_POSITION)

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)



class SublimeHaskellReinspectAll(sublime_plugin.WindowCommand):
    def run(self):
        autocompletion.clear_inspected()
        inspector.mark_all_files(self.window)




class SublimeHaskellSymbolInfoCommand(sublime_plugin.TextCommand):
    """
    Show information about selected symbol

    """
    def run(self, edit, filename = None, module_name = None, decl = None):
        if decl and filename:
            with autocompletion.database.files as files:
                if filename in files:
                    m = files[filename]
                    if decl in m.declarations:
                        self.show_symbol_info(m.declarations[decl])
                    else:
                        show_status_message('Symbol {0} not found in {1}'.format(decl, filename))
                else:
                    show_status_message('No info about module in {0}'.format(filename))
            return

        if decl and module_name:
            with autocompletion.database.get_cabal_modules() as cabal_modules:
                if module_name in cabal_modules:
                    m = cabal_modules[module_name]
                    if decl in m.declarations:
                        self.show_symbol_info(m.declarations[decl])
                    else:
                        show_status_message('Symbol {0} not found in {1}'.format(decl, filename))
                else:
                    show_status_message('No info about module {0}'.format(module_name))
            return

        (module_word, ident, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])
        full_name = '{0}.{1}'.format(module_word, ident) if module_word else ident

        current_file_name = self.view.file_name()

        candidates = []

        imported_symbol_not_found = False

        with autocompletion.database.symbols as decl_symbols:
            if ident in decl_symbols:

                decls = decl_symbols[ident] if not module_word else [d for d in decl_symbols[ident] if d.full_name() == full_name]

                modules_dict = symbols.declarations_modules(decls, lambda ms: symbols.get_visible_module(ms, current_file_name)).values()

                with autocompletion.database.files as files:
                    if current_file_name in files:
                        cur_info = files[current_file_name]

                        if not module_word or module_word == cur_info.name:
                            # this module declaration
                            candidates.extend([m.declarations[ident] for m in modules_dict if symbols.is_this_module(cur_info, m) and ident in m.declarations])
                        if not candidates:
                            # declarations from imported modules
                            candidates.extend([m.declarations[ident] for m in modules_dict if symbols.is_imported_module(cur_info, m, module_word) and ident in m.declarations])
                        if not candidates:
                            imported_symbol_not_found = True
                            # show all possible candidates
                            candidates.extend([m.declarations[ident] for m in modules_dict if ident in m.declarations])

                    # No info about imports for this file, just add all declarations
                    else:
                        candidates.extend([m.declarations[ident] for m in modules_dict if ident in m.declarations])

            else:
                imported_symbol_not_found = True

        if imported_symbol_not_found or not candidates:
            browse_for_module = False
            browse_module_candidate = None
            with autocompletion.database.modules as modules:
                if full_name in modules:
                    # Browse symbols in module
                    browse_for_module = True
                    browse_module_candidate = symbols.get_preferred_module(modules[full_name], current_file_name)

            if browse_for_module:
                if browse_module_candidate:
                    self.view.window().run_command('sublime_haskell_browse_module', {
                        'module_name': browse_module_candidate.name,
                        'filename': current_file_name })
                    return
                else:
                    show_status_message("No info about module {0}".format(full_name))
                    return
            elif not candidates:
                # Sometimes ghc-mod returns no info about module, but module exists
                # So there are no info about valid symbol
                # But if user sure, that symbol exists, he can force to call for ghci to get info
                import_list = []
                with autocompletion.database.files as files:
                    if current_file_name in files:
                        import_list.extend(files[current_file_name].imports.keys())

                if module_word:
                    # Full qualified name, just call to ghci_info
                    info = ghci_info(module_word, ident)
                    if info:
                        self.show_symbol_info(info)
                        return
                elif import_list:
                    # Allow user to select module
                    self.candidates = [(m, ident) for m in import_list]
                    self.view.window().show_quick_panel([
                        ['Select imported module', 'from where {0} may be imported'.format(ident)],
                        ['No, thanks']], self.on_import_selected)
                    return

                show_status_message('Symbol {0} not found'.format(ident), False)
                return

        if not candidates:
            show_status_message('Symbol {0} not found'.format(ident), False)
            return

        if len(candidates) == 1:
            self.show_symbol_info(candidates[0])
            return

        self.candidates = candidates
        self.view.window().show_quick_panel([[c.qualified_name()] for c in candidates], self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        self.show_symbol_info(self.candidates[idx])

    def on_import_selected(self, idx):
        if idx == 0: # Yes, select imported module
            self.view.window().show_quick_panel(['{0}.{1}'.format(i[0], i[1]) for i in self.candidates], self.on_candidate_selected)

    def on_candidate_selected(self, idx):
        if idx == -1:
            return

        (module_name, ident_name) = self.candidates[idx]
        info = util.symbol_info(self.view.file_name(), module_name, ident_name)
        if info:
            self.show_symbol_info(info)
        else:
            show_status_message("Can't get info for {0}.{1}".format(module_name, ident_name), False)

    def show_symbol_info(self, decl):
        output_view = self.view.window().get_output_panel('sublime_haskell_symbol_info')
        output_view.set_read_only(False)

        util.refine_decl(decl)

        # TODO: Move to separate command for Sublime Text 3
        output_view.run_command('sublime_haskell_output_text', {
            'text': decl.detailed() })

        output_view.sel().clear()
        output_view.set_read_only(True)

        self.view.window().run_command('show_panel', {
            'panel': 'output.' + 'sublime_haskell_symbol_info' })

    def browse_module(self, module):
        with autocompletion.database.modules as modules:
            decls = list(module.declarations.values())
            self.candidates = decls
            self.view.window().show_quick_panel([[decl.brief(), decl.docs.splitlines()[0]] if decl.docs else [decl.brief()] for decl in decls], self.on_done)

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)



class SublimeHaskellBrowseModule(sublime_plugin.WindowCommand):
    """
    Browse module symbols
    """
    def run(self, module_name = None, filename = None):
        if module_name:
            with autocompletion.database.modules as modules:
                if module_name not in modules:
                    show_status_message('Module {0} not found'.format(module_name), False)
                    return

                current_file_name = filename if filename else self.window.active_view().file_name()

                module_candidate = symbols.get_preferred_module(modules[module_name], current_file_name)

                if hdocs.load_module_docs(module_candidate):
                # FIXME: Not here!
                    cache.dump_cabal_cache(autocompletion.database, module_candidate.cabal)

                decls = list(module_candidate.declarations.values())
                self.candidates = sorted(decls, key = lambda d: d.brief())

                self.window.show_quick_panel([[decl.brief(), decl.docs.splitlines()[0]] if decl.docs else [decl.brief()] for decl in self.candidates], self.on_symbol_selected)
                return

        self.candidates = []

        with autocompletion.database.files as files:
            for fname, m in files.items():
                self.candidates.append([m.name, fname])

        with autocompletion.database.get_cabal_modules() as cabal_modules:
            for m in cabal_modules.values():
                self.candidates.append([m.name])

        self.candidates.sort(key = lambda c: c[0])

        self.window.show_quick_panel(self.candidates, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return

        module_name = self.candidates[idx][0]
        self.window.run_command('sublime_haskell_browse_module', {
            'module_name': module_name })

    def on_symbol_selected(self, idx):
        if idx == -1:
            return

        candidate = self.candidates[idx]

        if candidate.module.location:
            self.window.active_view().run_command('sublime_haskell_symbol_info', {
                'filename': candidate.module.location.filename,
                'decl': candidate.name })
        else:
            self.window.active_view().run_command('sublime_haskell_symbol_info', {
                'module_name': candidate.module.name,
                'decl': candidate.name })

class SublimeHaskellGoToDeclaration(sublime_plugin.TextCommand):
    def run(self, edit):
        (module_word, ident, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])

        full_name = '.'.join([module_word, ident]) if module_word else ident

        current_file_name = self.view.file_name()
        current_project = get_cabal_project_dir_of_file(current_file_name)

        module_candidates = []
        candidates = []

        with autocompletion.database.symbols as decl_symbols:
            if ident not in decl_symbols:
                show_status_message('Declaration for {0} not found'.format(ident), False)
                return

            decls = decl_symbols[ident]

            modules_dict = symbols.flatten(symbols.declarations_modules(decls, lambda ms: list(filter(symbols.is_by_sources, ms))).values())

            with autocompletion.database.files as files:
                if current_file_name in files:
                    cur_info = files[current_file_name]

                    if not module_word or module_word == cur_info.name:
                        # this module declarations
                        candidates.extend([m.declarations[ident] for m in modules_dict if symbols.is_this_module(cur_info, m) and ident in m.declarations])
                    if not candidates:
                        # declarations from imported modules within this project
                        candidates.extend([m.declarations[ident] for m in modules_dict if symbols.is_imported_module(cur_info, m, module_word) and symbols.is_within_project(m, cur_info.location.project) and ident in m.declarations])
                    if not candidates:
                        # declarations from imported modules within other projects
                        candidates.extend([m.declarations[ident] for m in modules_dict if symbols.is_imported_module(cur_info, m, module_word) and ident in m.declarations])
                    if not candidates:
                        # show all possible candidates
                        candidates.extend([m.declarations[ident] for m in modules_dict if ident in m.declarations])

                # No info about imports for this file, just add all declarations
                else:
                    candidates.extend([m.declarations[ident] for m in modules_dict if ident in declarations])

        with autocompletion.database.modules as modules:
            if full_name in modules:
                modules_list = list(filter(symbols.is_by_sources, modules[full_name]))

                # Find module in this project
                module_candidates.extend([m for m in modules_list if symbols.is_within_project(m, current_project)])
                if not module_candidates:
                    # Modules from other projects
                    module_candidates.extend(modules_list)

        if not candidates and not module_candidates:
            show_status_message('Declaration for {0} not found'.format(ident), False)
            return

        if len(candidates) + len(module_candidates) == 1:
            if len(module_candidates) == 1:
                self.view.window().open_file(module_candidates[0].location.filename)
                return
            if len(candidates) == 1:
                self.view.window().open_file(candidates[0].location.position(), sublime.ENCODED_POSITION)
                return

        # many candidates
        self.select_candidates = [([c.name, c.location.position()], True) for c in candidates] + [([m.name, m.location.filename], False) for m in module_candidates]
        self.view.window().show_quick_panel([c[0] for c in self.select_candidates], self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return

        selected = self.select_candidates[idx]
        if selected[1]:
            self.view.window().open_file(selected[0][1], sublime.ENCODED_POSITION)
        else:
            self.view.window().open_file(selected[0][1])

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)



class StandardInspectorAgent(threading.Thread):
    def __init__(self):
        super(StandardInspectorAgent, self).__init__()
        self.daemon = True

        self.modules_lock = threading.Lock()
        self.modules_to_load = []

        self.cabal_lock = threading.Lock()
        self.cabal_to_load = []

        self.module_docs = LockedObject([])

        self.update_event = threading.Event()

    def run(self):
        self.init_ghcmod_completions()
        self.load_module_completions()

        while True:
            load_modules = []
            with self.modules_lock:
                load_modules = self.modules_to_load
                self.modules_to_load = []

            cabal = current_cabal()

            if len(load_modules) > 0:
                try:
                    for m in load_modules:
                        self._load_standard_module(m, cabal)
                        # self._load_standard_module_docs(m, cabal)
                except:
                    continue

            load_module_docs = []
            with self.module_docs as module_docs:
                load_module_docs = module_docs[:]
                module_docs[:] = []

            if len(load_module_docs) > 0:
                for m in load_module_docs:
                    self._load_standard_module_docs(m, cabal)

            with self.cabal_lock:
                load_cabal = self.cabal_to_load
                self.cabal_to_load = []

            if len(load_cabal) > 0:
                try:
                    for c in load_cabal:
                        self.load_module_completions(c)
                except:
                    continue

            if len(load_modules) > 0:
                cache.dump_cabal_cache(autocompletion.database)

            self.update_event.wait(AGENT_SLEEP_TIMEOUT)
            self.update_event.clear()

    def load_module_info(self, module_name):
        with self.modules_lock:
            self.modules_to_load.append(module_name)
        self.update_event.set()

    def load_module_docs(self, module_name):
        with self.module_docs as module_docs:
            module_docs.append(module_name)
        self.update_event.set()

    def load_cabal_info(self, cabal_name = None):
        if not cabal_name:
            cabal_name = current_cabal()

        with self.cabal_lock:
            self.cabal_to_load.append(cabal_name)
        self.update_event.set()

    # Gets available LANGUAGE options and import modules from ghc-mod
    def init_ghcmod_completions(self):

        if not get_setting_async('enable_ghc_mod'):
            return

        # Init LANGUAGE completions
        autocompletion.language_completions = call_ghcmod_and_wait(['lang']).splitlines()

    # Load modules info for cabal/cabal-dev specified
    def load_module_completions(self, cabal = None):
        if not get_setting_async('enable_ghc_mod'):
            return

        if not cabal:
            cabal = current_cabal()


        try:
            with status_message_process('Loading standard modules info for {0}'.format(cabal)) as s:
                cache_begin_time = time.clock()
                cache.load_cabal_cache(autocompletion.database, cabal)
                cache_end_time = time.clock()
                log('loading standard modules cache for {0} within {1} seconds'.format(cabal, cache_end_time - cache_begin_time))

                modules = None
                with autocompletion.module_completions as module_completions:
                    if cabal in module_completions:
                        return
                    module_completions[cabal] = set(call_ghcmod_and_wait(['list'], cabal = cabal).splitlines())
                    modules = module_completions[cabal].copy()

                begin_time = time.clock()
                log('loading standard modules info for {0}'.format(cabal))

                loaded_modules = 0
                for m in modules:
                    self._load_standard_module(m, cabal)
                    # self._load_standard_module_docs(m, cabal)
                    loaded_modules += 1
                    s.percentage_message(loaded_modules, len(modules))

                end_time = time.clock()
                log('loading standard modules info for {0} within {1} seconds'.format(cabal, end_time - begin_time))

                cache.dump_cabal_cache(autocompletion.database, cabal)

        except Exception as e:
            log('loading standard modules info for {0} failed with {1}'.format(cabal, e))


    def _load_standard_module(self, module_name, cabal = None):
        if not cabal:
            cabal = current_cabal()

        with autocompletion.database.get_cabal_modules(cabal) as cabal_modules:
            if module_name in cabal_modules:
                return

        try:
            m = util.browse_module(module_name, cabal = cabal)
            if m:
                autocompletion.database.add_module(m)

        except Exception as e:
            log('Inspecting in-cabal module {0} failed: {1}'.format(module_name, e))

    def _load_standard_module_docs(self, module_name, cabal = None):
        if not cabal:
            cabal = current_cabal()

        with autocompletion.database.get_cabal_modules(cabal) as cabal_modules:
            if module_name in cabal_modules:
                try:
                    hdocs.load_module_docs(cabal_modules[module_name])

                except Exception as e:
                    log('Loading docs for in-cabal module {0} failed: {1}'.format(module_name, e))



std_inspector = None



class InspectorAgent(threading.Thread):
    def __init__(self):
        # Call the superclass constructor:
        super(InspectorAgent, self).__init__()
        # Make this thread daemonic so that it won't prevent the program
        # from exiting.
        self.daemon = True
        # Files that need to be re-inspected:
        self.dirty_files_lock = threading.Lock()
        self.dirty_files = []

        self.active_files = LockedObject([])

        # Event that is set (notified) when files have changed
        self.reinspect_event = threading.Event()

    CABALMSG = 'Compiling Haskell CabalInspector'
    MODULEMSG = 'Compiling Haskell ModuleInspector'

    def run(self):
        # Compile the CabalInspector:
        with status_message(InspectorAgent.CABALMSG) as s:

            exit_code, out, err = call_and_wait(['ghc',
                '--make', CABAL_INSPECTOR_SOURCE_PATH,
                '-o', CABAL_INSPECTOR_EXE_PATH,
                '-outputdir', CABAL_INSPECTOR_OBJ_DIR])

            if exit_code != 0:
                s.fail()
                error_msg = u"SublimeHaskell: Failed to compile CabalInspector\n{0}".format(err)
                wait_for_window(lambda w: self.show_errors(w, error_msg))
                # Continue anyway

        # Compile the ModuleInspector:
        with status_message(InspectorAgent.MODULEMSG) as s:

            exit_code, out, err = call_and_wait(['ghc',
                '--make', MODULE_INSPECTOR_SOURCE_PATH,
                '-package', 'ghc',
                '-o', MODULE_INSPECTOR_EXE_PATH,
                '-outputdir', MODULE_INSPECTOR_OBJ_DIR])

            if exit_code != 0:
                s.fail()
                error_msg = u"SublimeHaskell: Failed to compile ModuleInspector\n{0}".format(err)
                wait_for_window(lambda w: self.show_errors(w, error_msg))
                return

        # For first time, inspect all open folders and files
        wait_for_window(lambda w: self.mark_all_files(w))
        self.mark_active_files()

        # TODO: If compilation failed, we can't proceed; handle this.
        # Periodically wake up and see if there is anything to inspect.
        while True:
            files_to_reinspect = []
            files_to_doc = []
            with self.dirty_files_lock:
                files_to_reinspect = self.dirty_files
                self.dirty_files = []
            with self.active_files as active_files:
                files_to_doc = active_files[:]
                active_files[:] = []
            # Find the cabal project corresponding to each "dirty" file:
            cabal_dirs = []
            standalone_files = []
            for filename in files_to_reinspect:
                d = get_cabal_project_dir_of_file(filename)
                if d is not None:
                    cabal_dirs.append(d)
                else:
                    standalone_files.append(filename)
            # Eliminate duplicate project directories:
            cabal_dirs = list(set(cabal_dirs))
            standalone_files = list(set(standalone_files))
            for i, d in enumerate(cabal_dirs):
                self._refresh_all_module_info(d, i + 1, len(cabal_dirs))
            for f in standalone_files:
                self._refresh_module_info(f)
            for f in files_to_doc:
                with autocompletion.database.files as files:
                    if f in files:
                        for i in files[f].imports.values():
                            std_inspector.load_module_docs(i.module)
            self.reinspect_event.wait(AGENT_SLEEP_TIMEOUT)
            self.reinspect_event.clear()

    def mark_active_files(self):
        def mark_active_files_():
            for w in sublime.windows():
                for v in w.views():
                    with self.active_files as active_files:
                        active_files.append(v.file_name())
        sublime.set_timeout(lambda: mark_active_files_, 0)
        self.reinspect_event.set()

    def mark_all_files(self, window):
        folder_files = []
        for folder in window.folders():
            folder_files.extend(list_files_in_dir_recursively(folder))
        with self.dirty_files_lock:
            self.dirty_files.extend([f for f in folder_files if f.endswith('.hs')])
        self.reinspect_event.set()

    def show_errors(self, window, error_text):
        sublime.set_timeout(lambda: output_error(window, error_text), 0)

    def mark_file_active(self, filename):
        with self.active_files as active_files:
            active_files.append(filename)
        self.reinspect_event.set()

    def mark_file_dirty(self, filename):
        "Report that a file should be reinspected."
        with self.dirty_files_lock:
            self.dirty_files.append(filename)
        self.reinspect_event.set()

    def _refresh_all_module_info(self, cabal_dir, index, count):
        "Rebuild module information for all files under the specified directory."
        begin_time = time.clock()
        log('reinspecting project ({0})'.format(cabal_dir))
        # Process all files within the Cabal project:
        # TODO: Only process files within the .cabal file's "src" directory.
        (project_name, cabal_file) = get_cabal_in_dir(cabal_dir)

        with status_message_process('Reinspecting ({0}/{1}) {2}'.format(index, count, project_name), priority = 1) as s:
            cache.load_project_cache(autocompletion.database, cabal_dir)

            # set project and read cabal
            if cabal_file and project_name:
                self._refresh_project_info(cabal_dir, project_name, cabal_file)

            files_in_dir = list_files_in_dir_recursively(cabal_dir)
            haskell_source_files = [x for x in files_in_dir if x.endswith('.hs') and ('dist/build/autogen' not in x)]
            filenames_loaded = 0
            for filename in haskell_source_files:
                self._refresh_module_info(filename, False)
                filenames_loaded += 1
                s.percentage_message(filenames_loaded, len(haskell_source_files))
            end_time = time.clock()
            log('total inspection time: {0} seconds'.format(end_time - begin_time))

            cache.dump_project_cache(autocompletion.database, cabal_dir)

    def _refresh_project_info(self, cabal_dir, project_name, cabal_file):
        exit_code, out, err = call_and_wait(
            [CABAL_INSPECTOR_EXE_PATH, cabal_file])

        if exit_code == 0:
            new_info = json.loads(out)

            if 'error' not in new_info:
                if 'executables' in new_info and 'library' in new_info:
                    with autocompletion.projects as projects:
                        projects[project_name] = {
                            'dir': cabal_dir,
                            'cabal': os.path.basename(cabal_file),
                            'library': new_info['library'],
                            'executables': new_info['executables'],
                            'tests': new_info['tests'] }

    def _refresh_module_info(self, filename, standalone = True):
        "Rebuild module information for the specified file."
        # TODO: Only do this within Haskell files in Cabal projects.
        # TODO: Currently the ModuleInspector only delivers top-level functions
        #       with hand-written type signatures. This code should make that clear.
        # If the file hasn't changed since it was last inspected, do nothing:
        if not filename.endswith('.hs'):
            return

        with autocompletion.database.files as files:
            if filename in files:
                last_inspection_time = files[filename].last_inspection_time
                modification_time = os.stat(filename).st_mtime
                # Skip if we already inspected after last modification
                if modification_time <= last_inspection_time:
                    # log('skipping inspecting %s' % filename)
                    return
                else:
                    files[filename].last_inspection_time = time.time()

        ghc_opts = get_ghc_opts()
        ghc_opts_args = [' '.join(ghc_opts)] if ghc_opts else []

        exit_code, stdout, stderr = call_and_wait(
            [MODULE_INSPECTOR_EXE_PATH, filename] + ghc_opts_args, cwd = get_source_dir(filename))

        module_inspector_out = MODULE_INSPECTOR_RE.search(stdout)

        # Update only when module is ok
        if exit_code == 0 and module_inspector_out:
            new_info = json.loads(module_inspector_out.group('result'))

            if 'error' not in new_info:
                # # Load standard modules
                if 'imports' in new_info:
                    for mi in new_info['imports']:
                        if 'importName' in mi:
                            std_inspector.load_module_info(mi['importName'])

                try:
                    def make_import(import_info):
                        import_name = import_info['importName']
                        ret = symbols.Import(import_name, import_info['qualified'], import_info['as'])
                        return (import_name, ret)

                    module_imports = dict(map(make_import, new_info['imports']))
                    import_list = new_info['exportList'] if ('exportList' in new_info and new_info['exportList'] is not None) else []

                    new_module = symbols.Module(new_info['moduleName'], import_list, module_imports, {}, symbols.module_location(filename), last_inspection_time=time.time())
                    for d in new_info['declarations']:
                        location = symbols.Location(filename, d['line'], d['column'])
                        if d['what'] == 'function':
                            new_function = symbols.Function(d['name'], d['type'], d['docs'], location, new_module)
                            util.refine_type(new_function)
                            new_module.add_declaration(new_function)
                        elif d['what'] == 'type':
                            new_module.add_declaration(symbols.Type(d['name'], d['context'], d['args'], None, d['docs'], location))
                        elif d['what'] == 'newtype':
                            new_module.add_declaration(symbols.Newtype(d['name'], d['context'], d['args'], None, d['docs'], location))
                        elif d['what'] == 'data':
                            new_module.add_declaration(symbols.Data(d['name'], d['context'], d['args'], None, d['docs'], location))
                        elif d['what'] == 'class':
                            new_module.add_declaration(symbols.Class(d['name'], d['context'], d['args'], d['docs'], location))
                        else:
                            new_module.add_declaration(symbols.Declaration(d['name'], 'declaration', d['docs'], location))

                    autocompletion.database.add_file(filename, new_module)

                    if standalone:
                        # Do we need save cache for standalone files?
                        pass

                    for i in new_module.imports.values():
                        std_inspector.load_module_info(i.module)

                except Exception as e:
                    log('Inspecting file {0} failed: {1}'.format(filename, e))

            else:
                log('ModuleInspector returns error: {0}'.format(new_info['error']))

        else:
            log('ModuleInspector exited with code {0}. Stderr: {1}'.format(exit_code, stderr))


def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files



inspector = None



class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    def __init__(self):
        self.local_settings = {
            'enable_ghc_mod': None,
            'use_cabal_dev': None,
            'cabal_dev_sandbox': None,
        }

        for s in self.local_settings.keys():
            self.local_settings[s] = get_setting(s)

        # Subscribe to settings changes to update data
        get_settings().add_on_change('enable_ghc_mod', lambda: self.on_setting_changed())

    def on_setting_changed(self):
        same = True
        for k, v in self.local_settings.items():
            r = get_setting(k)
            same = same and v == r
            self.local_settings[k] = r

        # Update cabal status of active view
        window = sublime.active_window()
        if window:
            view = window.active_view()
            if view:
                self.set_cabal_status(view)

        if not same:
            # TODO: Changed completion settings! Update autocompletion data properly
            # For now at least try to load cabal modules info
            std_inspector.load_cabal_info()
            pass

    def get_special_completions(self, view, prefix, locations):

        # Contents of the current line up to the cursor
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if get_setting('auto_complete_language_pragmas'):
            # TODO handle multiple selections
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                return [(unicode(c),) * 2 for c in autocompletion.language_completions]

        # Autocompletion for import statements
        if get_setting('auto_complete_imports'):
            match_import = IMPORT_RE.match(line_contents)
            if match_import:
                import_completions = [(unicode(c),) * 2 for c in autocompletion.get_current_module_completions()]

                # Right after "import "? Propose "qualified" as well!
                qualified_match = IMPORT_QUALIFIED_POSSIBLE_RE.match(line_contents)
                if qualified_match:
                    qualified_prefix = qualified_match.group('qualifiedprefix')
                    if qualified_prefix == "" or "qualified".startswith(qualified_prefix):
                        import_completions.insert(0, (u"qualified", "qualified "))

                return import_completions

        return None

    def on_query_completions(self, view, prefix, locations):
        if not is_haskell_source(view):
            return []

        begin_time = time.clock()
        # Only suggest symbols if the current file is part of a Cabal project.

        completions = autocompletion.get_import_completions(view, prefix, locations)

        if not completions:
            completions = autocompletion.get_completions(view, prefix, locations)

        end_time = time.clock()
        log('time to get completions: {0} seconds'.format(end_time - begin_time))
        # Don't put completions with special characters (?, !, ==, etc.)
        # into completion because that wipes all default Sublime completions:
        # See http://www.sublimetext.com/forum/viewtopic.php?t=8659
        # TODO: work around this
        comp = [c for c in completions if NO_SPECIAL_CHARS_RE.match(c[0].split('\t')[0])]
        if get_setting('inhibit_completions') and len(comp) != 0:
            return (comp, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        return comp

    def set_cabal_status(self, view):
        filename = view.file_name()
        if filename:
            (cabal_dir, project_name) = get_cabal_project_dir_and_name_of_file(filename)
            cabal = 'cabal-dev' if get_setting_async('use_cabal_dev') else 'cabal'
            if project_name:
                view.set_status('sublime_haskell_cabal', '{0}: {1}'.format(cabal, project_name))

    def on_new(self, view):
        self.set_cabal_status(view)
        if is_haskell_source(view):
            filename = view.file_name()
            inspector.mark_file_dirty(filename)
            inspector.mark_file_active(filename)

    def on_load(self, view):
        self.set_cabal_status(view)
        if is_haskell_source(view):
            filename = view.file_name()
            inspector.mark_file_dirty(filename)
            inspector.mark_file_active(filename)

    def on_activated(self, view):
        self.set_cabal_status(view)

    def on_post_save(self, view):
        if is_haskell_source(view):
            inspector.mark_file_dirty(view.file_name())

    def on_query_context(self, view, key, operator, operand, match_all):
        if key == 'auto_completion_popup':
            return get_setting('auto_completion_popup')
        elif key == 'is_haskell_source':
            return is_haskell_source(view)
        elif key == "is_module_completion" or key == "is_import_completion":
            chars = {
                "is_module_completion": '.',
                "is_import_completion": '(' }

            region = view.sel()[0]
            if region.a != region.b:
                return False
            word_region = view.word(region)
            preline = get_line_contents_before_region(view, word_region)
            preline += chars[key]
            return can_complete_qualified_symbol(get_qualified_symbol(preline))
        else:
            return False



def plugin_loaded():
    global MODULE_INSPECTOR_SOURCE_PATH
    global MODULE_INSPECTOR_EXE_PATH
    global MODULE_INSPECTOR_OBJ_DIR
    global CABAL_INSPECTOR_SOURCE_PATH
    global CABAL_INSPECTOR_EXE_PATH
    global CABAL_INSPECTOR_OBJ_DIR

    package_path = sublime_haskell_package_path()
    cache_path = sublime_haskell_cache_path()

    MODULE_INSPECTOR_SOURCE_PATH = os.path.join(package_path, 'ModuleInspector.hs')
    MODULE_INSPECTOR_EXE_PATH = os.path.join(cache_path, 'ModuleInspector')
    MODULE_INSPECTOR_OBJ_DIR = os.path.join(cache_path, 'obj/ModuleInspector')
    CABAL_INSPECTOR_SOURCE_PATH = os.path.join(package_path, 'CabalInspector.hs')
    CABAL_INSPECTOR_EXE_PATH = os.path.join(cache_path, 'CabalInspector')
    CABAL_INSPECTOR_OBJ_DIR = os.path.join(cache_path, 'obj/CabalInspector')

    if get_setting('inspect_modules'):
        global std_inspector
        std_inspector = StandardInspectorAgent()
        std_inspector.start()

        global inspector
        inspector = InspectorAgent()
        inspector.start()

    # TODO: How to stop_hdevtools() in Sublime Text 2?
    start_hdevtools()

def plugin_unloaded():
    # Does this work properly on exit?
    stop_hdevtools()

if int(sublime.version()) < 3000:
    plugin_loaded()
