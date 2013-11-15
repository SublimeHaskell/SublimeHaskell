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
    import hsdev
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.symbols as symbols
    import SublimeHaskell.cache as cache
    import SublimeHaskell.util as util
    import SublimeHaskell.hdocs as hdocs
    from SublimeHaskell.ghci import ghci_info
    from SublimeHaskell.haskell_docs import haskell_docs
    from SublimeHaskell.hdevtools import start_hdevtools, stop_hdevtools
    import SublimeHaskell.hsdev as hsdev


# If true, files that have not changed will not be re-inspected.
CHECK_MTIME = True

MODULE_INSPECTOR_SOURCE_PATH = None
MODULE_INSPECTOR_EXE_PATH = None
MODULE_INSPECTOR_OBJ_DIR = None
CABAL_INSPECTOR_SOURCE_PATH = None
CABAL_INSPECTOR_EXE_PATH = None
CABAL_INSPECTOR_OBJ_DIR = None
INSPECTOR_ENABLED = False
INSPECTOR_RUNNING = False

HSDEV_CACHE_PATH = None

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

    def get_completions(self, view, prefix, locations):
        "Get all the completions that apply to the current file."

        current_file_name = view.file_name()

        if not current_file_name:
            return []

        self.current_filename = current_file_name
        line_contents = get_line_contents(view, locations[0])
        (qualified_module, symbol_name, is_import_list) = get_qualified_symbol(line_contents)
        qualified_prefix = '{0}.{1}'.format(qualified_module, symbol_name) if qualified_module else symbol_name

        suggestions = []
        if is_import_list:
            current_project = hsdev.module(file = current_file_name).location.project
            if current_project:
                project_modules = [m.name for m in hsdev.list_modules(project = current_project)]
                if qualified_module in project_modules:
                    suggestions = hsdev.module(name = qualified_module, project = current_project).declarations.values()
            if not suggestions:
                suggestions = hsdev.module(name = qualified_module, cabal = 'cabal').declarations.values()
        else:
            suggestions = hsdev.complete(qualified_prefix, current_file_name) or []

        return list(set([s.suggest() for s in suggestions]))

    def completions_for_module(self, module, filename = None):
        """
        Returns completions for module
        """
        if not module:
            return []
        return map(lambda d: d.suggest(), hsdev.module(name = module, file = filename).declarations.values())

    def completions_for(self, module_name, filename = None):
        """
        Returns completions for module
        """
        with self.database.modules as modules:
            if module_name not in modules:
                return []
            # TODO: Show all possible completions?
            return self.completions_for_module(module_name, filename)

    def get_import_completions(self, view, prefix, locations):

        self.current_filename = view.file_name()
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if get_setting_async('auto_complete_language_pragmas'):
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
        return set([m.name for m in hsdev.scope_modules(file = self.current_filename, cabal = current_cabal())])


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

        decls = hsdev.symbol('', cabal = current_cabal())
        decls.extend(hsdev.symbol('', source = True))

        for decl in decls:
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

        decls = hsdev.symbol('', source = True)

        for decl in decls:
            self.files.append([decl.location.filename, str(decl.location.line), str(decl.location.column)])
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
        global INSPECTOR_ENABLED

        if INSPECTOR_ENABLED:
            autocompletion.clear_inspected()
            inspector.mark_all_files(self.window)
        else:
            show_status_message("inspector_enabled setting is false", isok=False)


class SublimeHaskellSymbolInfoCommand(sublime_plugin.TextCommand):
    """
    Show information about selected symbol

    """
    def run(self, edit, filename = None, module_name = None, decl = None):
        if decl and (filename or module_name):
            self.full_name = decl
            self.candidates = hsdev.symbol(decl, file = self.current_file_name, module = self.module_name)
        else:
            (module_word, ident, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            self.full_name = '{0}.{1}'.format(module_word, ident) if module_word else ident

            self.current_file_name = self.view.file_name()

            candidates = hsdev.whois(self.full_name, self.current_file_name)

            if not candidates:
                candidates = hsdev.lookup(self.full_name, self.current_file_name)

        if not self.candidates:
            show_status_message('Symbol {0} not found'.format(self.full_name))
            return

        if len(self.candidates) == 1:
            self.show_symbol_info(self.candidates[0])
            return

        self.view.window().show_quick_panel([[c.qualified_name()] for c in self.candidates], self.on_done)

            # if browse_for_module:
            #     if browse_module_candidate:
            #         self.view.window().run_command('sublime_haskell_browse_module', {
            #             'module_name': browse_module_candidate.name,
            #             'filename': current_file_name })
            #         return

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
        info = hsdev.whois('{0}.{1}'.format(module_name, ident_name), self.view.file_name())

        if info:
            self.show_symbol_info(info[0])
        else:
            show_status_message("Can't get info for {0}.{1}".format(module_name, ident_name), False)

    def show_symbol_info(self, decl):
        output_view = self.view.window().get_output_panel('sublime_haskell_symbol_info')
        output_view.set_read_only(False)

        # TODO: Move to separate command for Sublime Text 3
        output_view.run_command('sublime_haskell_output_text', {
            'text': decl.detailed() })

        output_view.sel().clear()
        output_view.set_read_only(True)

        self.view.window().run_command('show_panel', {
            'panel': 'output.' + 'sublime_haskell_symbol_info' })

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)


class SublimeHaskellInsertImportForSymbol(sublime_plugin.TextCommand):
    """
    Insert import for symbol
    """
    def run(self, edit, filename = None, decl = None):
        self.full_name = decl
        self.current_file_name = filename
        self.edit = edit

        if not self.current_file_name:
            self.current_file_name = self.view.file_name()

        if not self.full_name:
            (module_word, ident, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            self.full_name = '{0}.{1}'.format(module_word, ident) if module_word else ident

        if hsdev.whois(self.full_name, file = self.current_file_name):
            show_status_message('Symbol {0} already in scope'.format(self.full_name))
            return

        self.candidates = hsdev.lookup(self.full_name, file = self.current_file_name)

        if not self.candidates:
            show_status_message('Symbol {0} not found'.format(self.full_name))
            return

        if len(self.candidates) == 1:
            self.add_import(self.candidates[0])
            return

        self.view.window().show_quick_panel([[c.qualified_name()] for c in self.candidates], self.on_done)

    def add_import(self, decl):
        cur_module = hsdev.module(file = self.current_file_name)
        imports = sorted(cur_module.imports.values(), key = lambda i: i.location.line)
        after = [i for i in imports if i.module > decl.module.name]

        insert_line = 0
        insert_gap = False

        if len(after) > 0:
            # Insert before after[0]
            insert_line = after[0].location.line - 1
        elif len(imports) > 0:
            # Insert after all imports
            insert_line = imports[-1].location.line
        elif len(cur_module.declarations) > 0:
            # Insert before first declaration
            insert_line = min([d.location.line for d in cur_module.declarations.values()]) - 1
            insert_gap = True
        else:
            # Insert at the end of file
            insert_line = self.view.rowcol(self.view.size())[0]

        insert_text = 'import {0}\n'.format(decl.module.name) + ('\n' if insert_gap else '')

        pt = self.view.text_point(insert_line, 0)
        self.view.insert(self.edit, pt, insert_text)

    def on_done(self, idx):
        if idx == -1:
            return
        self.add_import(self.candidates[idx])


class SublimeHaskellBrowseModule(sublime_plugin.WindowCommand):
    """
    Browse module symbols
    """
    def run(self, module_name = None, filename = None):
        if module_name or filename:
            m = hsdev.module(name = module_name, file = filename)
            if not m:
                show_status_message('Module {0} not found'.format(module_name or filename))
                return

            decls = list(m.declarations.values())
            self.candidates = sorted(decls, key = lambda d: d.brief())

            self.window.show_quick_panel([[decl.brief(), decl.docs.splitlines()[0]] if decl.docs else [decl.brief()] for decl in self.candidates], self.on_symbol_selected)
            return

        self.candidates = []

        self.candidates.extend([[m.name] for m in hsdev.list_modules(cabal = current_cabal())])
        self.candidates.extend([[m.name] for m in hsdev.list_modules(source = True)])

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
            log('data is {0}'.format({
                'module_name': candidate.module.name,
                'decl': candidate.name }))
            self.window.active_view().run_command('sublime_haskell_symbol_info', {
                'module_name': candidate.module.name,
                'decl': candidate.name })

class SublimeHaskellGoToDeclaration(sublime_plugin.TextCommand):
    def run(self, edit):
        (module_word, ident, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])

        full_name = '.'.join([module_word, ident]) if module_word else ident

        current_file_name = self.view.file_name()
        current_project = get_cabal_project_dir_of_file(current_file_name)

        candidate = hsdev.whois(full_name, current_file_name)

        if candidate and candidate.location and candidate.location.filename:
            self.view.window().open_file(candidate.location.position(), sublime.ENCODED_POSITION)
            return

        candidates = hsdev.symbol(full_name, source = True)

        if not candidates:
            show_status_message('Declaration {0} not found'.format(ident), False)
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
        self.modules_to_load = []
        self.modules_lock = threading.Lock()
        self.cabal_to_load = []
        self.cabal_lock = threading.Lock()
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

            with self.cabal_lock:
                load_cabal = self.cabal_to_load
                self.cabal_to_load = []

            if len(load_cabal) > 0:
                try:
                    for c in load_cabal:
                        self.load_module_completions(c)
                except:
                    continue

            self.update_event.wait(AGENT_SLEEP_TIMEOUT)
            self.update_event.clear()

    def load_module_info(self, module_name):
        with self.modules_lock:
            self.modules_to_load.append(module_name)
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
        # autocompletion.language_completions = call_ghcmod_and_wait(['lang']).splitlines()

    # Load modules info for cabal/cabal-dev specified
    def load_module_completions(self, cabal = None):
        if not cabal:
            cabal = current_cabal()

        try:
            with status_message_process('Loading standard modules info for {0}'.format(cabal)) as s:
                cache_begin_time = time.clock()
                hsdev.load(path = HSDEV_CACHE_PATH)
                cache_end_time = time.clock()
                log('loading standard modules cache for {0} within {1} seconds'.format(cabal, cache_end_time - cache_begin_time))

                begin_time = time.clock()
                log('loading standard modules info for {0}'.format(cabal))

                def module_scanned(msg):
                    s.percentage_message(msg['progress']['current'], msg['progress']['total'])

                hsdev.scan(cabal = cabal, wait = True, on_status = module_scanned)

                end_time = time.clock()
                log('loading standard modules info for {0} within {1} seconds'.format(cabal, end_time - begin_time))

                # hsdev.save_cache(path = HSDEV_CACHE_PATH)

        except Exception as e:
            log('loading standard modules info for {0} failed with {1}'.format(cabal, e))


    def _load_standard_module(self, module_name, cabal = None):
        if not cabal:
            cabal = current_cabal()

        try:
            hsdev.scan(cabal = cabal, modules = [module_name])
        except Exception as e:
            log('Inspecting in-cabal module {0} failed: {1}'.format(module_name, e))



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

    def run(self):
        hsdev.load(path = HSDEV_CACHE_PATH)

        wait_for_window(lambda w: self.mark_all_files(w))
        self.mark_active_files()

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
                # Why do we need reparse full project after each change?
                # if d is not None:
                #     cabal_dirs.append(d)
                # else:
                #     standalone_files.append(filename)
                standalone_files.append(filename)
            # Eliminate duplicate project directories:
            cabal_dirs = list(set(cabal_dirs))
            standalone_files = list(set(standalone_files))
            for i, d in enumerate(cabal_dirs):
                self._refresh_all_module_info(d, i + 1, len(cabal_dirs))
            for f in standalone_files:
                self._refresh_module_info(f)

            # hsdev.save_cache(path = HSDEV_CACHE_PATH)

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
        output_error_async(window, error_text)

    def mark_file_active(self, filename):
        with self.active_files as active_files:
            active_files.append(filename)
        self.reinspect_event.set()

    def mark_file_dirty(self, filename):
        "Report that a file should be reinspected."
        if not filename:
            return

        with self.dirty_files_lock:
            self.dirty_files.append(filename)
        self.reinspect_event.set()

    def _refresh_all_module_info(self, cabal_dir, index, count):
        begin_time = time.clock()
        log('reinspecting project ({0})'.format(cabal_dir))
        (project_name, cabal_file) = get_cabal_in_dir(cabal_dir)

        try:

            with status_message_process('Reinspecting ({0}/{1}) {2}'.format(index, count, project_name), priority = 1) as s:

                def file_scanned(msg):
                    s.percentage_message(msg['progress']['current'], msg['progress']['total'])

                hsdev.scan(projects = [cabal_dir], wait = True, on_status = file_scanned)

                end_time = time.clock()
                log('total inspection time: {0} seconds'.format(end_time - begin_time))

        except Exception as e:
            log('Inspecting project {0} failed: {1}'.format(cabal_dir, e))

    def _refresh_module_info(self, filename, standalone = True):
        if standalone:
            hsdev.scan(files = [filename])


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
        global INSPECTOR_ENABLED

        INSPECTOR_ENABLED = get_setting('inspect_modules')

        # Start the inspector if needed
        # TODO Also stop it if needed!
        if INSPECTOR_ENABLED and not INSPECTOR_RUNNING:
            start_inspector()
        elif (not INSPECTOR_ENABLED) and INSPECTOR_RUNNING:
            # TODO Implement stopping it
            log('The ModuleInspector cannot be stopped as of now. You have to restart Sublime for that.')

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

        if INSPECTOR_ENABLED and not same:
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
        global INSPECTOR_ENABLED

        self.set_cabal_status(view)
        if is_haskell_source(view):
            filename = view.file_name()
            if INSPECTOR_ENABLED:
                inspector.mark_file_dirty(filename)
                inspector.mark_file_active(filename)

    def on_load(self, view):
        global INSPECTOR_ENABLED

        self.set_cabal_status(view)
        if is_haskell_source(view):
            filename = view.file_name()
            if INSPECTOR_ENABLED:
                inspector.mark_file_dirty(filename)
                inspector.mark_file_active(filename)

    def on_activated(self, view):
        self.set_cabal_status(view)

    def on_post_save(self, view):
        global INSPECTOR_ENABLED

        if is_haskell_source(view):
            if INSPECTOR_ENABLED:
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


def start_inspector():
    global INSPECTOR_RUNNING

    if INSPECTOR_RUNNING:
        raise Exception('SublimeHaskell: ModuleInspector is already running!')

    log('starting ModuleInspector')

    global std_inspector
    std_inspector = StandardInspectorAgent()
    std_inspector.start()

    global inspector
    inspector = InspectorAgent()
    inspector.start()

    INSPECTOR_RUNNING = True


def plugin_loaded():
    global MODULE_INSPECTOR_SOURCE_PATH
    global MODULE_INSPECTOR_EXE_PATH
    global MODULE_INSPECTOR_OBJ_DIR
    global CABAL_INSPECTOR_SOURCE_PATH
    global CABAL_INSPECTOR_EXE_PATH
    global CABAL_INSPECTOR_OBJ_DIR
    global OUTPUT_PATH
    global HSDEV_CACHE_PATH
    global INSPECTOR_ENABLED
    global INSPECTOR_RUNNING

    package_path = sublime_haskell_package_path()
    cache_path = sublime_haskell_cache_path()

    MODULE_INSPECTOR_SOURCE_PATH = os.path.join(package_path, 'ModuleInspector.hs')
    MODULE_INSPECTOR_EXE_PATH = os.path.join(cache_path, 'ModuleInspector')
    MODULE_INSPECTOR_OBJ_DIR = os.path.join(cache_path, 'obj/ModuleInspector')
    CABAL_INSPECTOR_SOURCE_PATH = os.path.join(package_path, 'CabalInspector.hs')
    CABAL_INSPECTOR_EXE_PATH = os.path.join(cache_path, 'CabalInspector')
    CABAL_INSPECTOR_OBJ_DIR = os.path.join(cache_path, 'obj/CabalInspector')
    INSPECTOR_ENABLED = get_setting('inspect_modules')

    if INSPECTOR_ENABLED:
        start_inspector()

    # TODO: How to stop_hdevtools() in Sublime Text 2?
    start_hdevtools()

def plugin_unloaded():
    # Does this work properly on exit?
    stop_hdevtools()

if int(sublime.version()) < 3000:
    plugin_loaded()
