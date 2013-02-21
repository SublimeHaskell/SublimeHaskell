import json
import os
import re
import sublime
import sublime_plugin
import threading
import time

from sublime_haskell_common import *
import symbols
from ghci import ghci_info, ghci_info_symbol
from haskell_docs import haskell_docs

# If true, files that have not changed will not be re-inspected.
CHECK_MTIME = True

MODULE_INSPECTOR_SOURCE_PATH = os.path.join(PACKAGE_PATH, 'ModuleInspector.hs')
MODULE_INSPECTOR_EXE_PATH = os.path.join(PACKAGE_PATH, 'ModuleInspector')
MODULE_INSPECTOR_OBJ_DIR = os.path.join(PACKAGE_PATH, 'obj')
CABAL_INSPECTOR_SOURCE_PATH = os.path.join(PACKAGE_PATH, 'CabalInspector.hs')
CABAL_INSPECTOR_EXE_PATH = os.path.join(PACKAGE_PATH, 'CabalInspector')
CABAL_INSPECTOR_OBJ_DIR = os.path.join(PACKAGE_PATH, 'obj')

OUTPUT_PATH = os.path.join(PACKAGE_PATH, 'module_info.cache')

# The agent sleeps this long between inspections.
AGENT_SLEEP_DURATION = 5.0

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
    Returns the contents of the line at the given location.
    """
    return view.substr(sublime.Region(view.line(location).a, location))


def get_line_contents_before_region(view, region):
    """
    Returns the of the line before the given region (including it).
    """
    return view.substr(sublime.Region(view.line(region).a, region.b))

def get_qualified_name(s):
    """
    'bla bla bla Data.List.fo' -> ('Data.List', 'Data.List.fo')
    """
    if len(s) == 0:
        return ('', '')
    quals = s.split()[-1].split('.')
    filtered = map(lambda s: filter(lambda c: c.isalpha() or c.isdigit() or c == '_', s), quals)
    return ('.'.join(filtered[0:len(filtered) - 1]), '.'.join(filtered))

def get_qualified_symbol(line):
    """
    Get module context of symbol and symbol itself
    Returns (module, name), where module (or one of) can be None
    """
    res = IMPORT_SYMBOL_RE.search(line)
    if res:
        return (res.group('module'), res.group('identifier'))
    res = SYMBOL_RE.search(line)
    # res always match
    return (res.group('module'), res.group('identifier'))

# Autocompletion data
class AutoCompletion(object):
    """Information for completion"""
    def __init__(self):
        self.language_completions = []
        self.module_completions = set()

        # Currently used projects
        # name => project where project is:
        #   dir - project dir
        #   cabal - cabal file
        #   executables - list of executables where executable is
        #     name - name of executable
        self.projects_lock = threading.Lock()
        self.projects = {}

        # Storage of information
        self.database = symbols.Database()

        # keywords
        # TODO: keywords can't appear anywhere, we can suggest in right places
        self.keyword_completions = map(
            lambda k: (k + '\t(keyword)', k),
            ['case', 'data', 'instance', 'type', 'where', 'deriving', 'import', 'module'])

    def clear_inspected(self):
        # self.info = {}
        # self.std_info = {}
        self.projects = {}
        self.database = symbols.Database()

    def unalias_module_name(self, view, alias):
        "Get module names by alias"
        current_file_name = view.file_name()
        with self.database.files_lock:
            if current_file_name in self.database.files:
                return self.database.files[current_file_name].unalias(alias)
        return []

    def get_completions(self, view, prefix, locations):
        "Get all the completions that apply to the current file."

        current_file_name = view.file_name()

        # Contents of the line under the first cursor
        line_contents = get_line_contents(view, locations[0])

        # If the current line is an import line, gives us (My.Module, ident)
        (qualified_module, symbol_name) = get_qualified_symbol(line_contents)
        qualified_prefix = '{0}.{1}'.format(qualified_module, symbol_name) if qualified_module else symbol_name

        # The list of completions we're going to assemble
        completions = []

        # Complete with modules too
        if qualified_module:
            completions.extend(self.get_module_completions_for(qualified_prefix))

        moduleImports = []

        with self.database.files_lock:
            if current_file_name in self.database.files:
                cur_info = self.database.files[current_file_name]

                if qualified_module:
                    # If symbol is qualified, use completions from module specified
                    moduleImports.append(qualified_module)
                    moduleImports.extend([i.module for i in cur_info.imports.values() if i.import_as == qualified_module])
                else:
                    # Otherwise, use completions from all importred unqualified modules and from this module
                    moduleImports.append(cur_info.name)
                    moduleImports.append('Prelude')
                    moduleImports.extend([i.module for i in cur_info.imports.values() if not i.is_qualified])
                    # Add keyword completions and module completions
                    completions.extend(self.keyword_completions)
                    completions.extend(self.get_module_completions_for(qualified_prefix, [i.module for i in cur_info.imports.values()]))

        for mi in set(moduleImports):
            completions.extend(self.completions_for(mi, current_file_name))

        return list(set(completions))

    def completions_for(self, module_name, filename = None):
        """
        Returns completions for module
        """
        with self.database.modules_lock:
            if module_name not in self.database.modules:
                return []
            # TODO: Show all possible completions?
            module = symbols.get_visible_module(self.database.modules[module_name], filename)
            if not module:
                return []
            return map(lambda d: d.suggest(), module.declarations.values())

    def get_import_completions(self, view, prefix, locations):

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

                import_list_completions.extend(self.completions_for(module_name, view.file_name()))

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

        module_list = modules if modules else self.module_completions
        return list(set((module_next_name(m) + '\t(module)', module_next_name(m)) for m in module_list if m.startswith(qualified_prefix)))



autocompletion = AutoCompletion()



class SublimeHaskellComplete(sublime_plugin.TextCommand):
    """ Shows autocompletion popup """
    def run(self, edit, characters):
        for region in self.view.sel():
            self.view.insert(edit, region.end(), characters)
        line = get_line_contents_before_region(self.view, self.view.sel()[0])
        (module_name, symbol_name) = get_qualified_symbol(line)
        if module_name and module_name in autocompletion.module_completions:
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
        self.names = []
        self.declarations = []

        # (module, ident) => symbols.Declaration
        decls = {}

        with autocompletion.database.cabal_modules_lock:
            for m in autocompletion.database.get_cabal_modules().values():
                for decl in m.declarations.values():
                    decls[(m.name, decl.name)] = decl

        with autocompletion.database.files_lock:
            for m in autocompletion.database.files.values():
                for decl in m.declarations.values():
                    decls[(m.name, decl.name)] = decl

        for decl in decls.values():
            self.names.append(decl.name)
            self.declarations.append(decl.brief())

        self.window.show_quick_panel(self.declarations, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        view = self.window.active_view()
        if not view:
            return
        edit = view.begin_edit()
        for r in view.sel():
            view.replace(edit, r, self.names[idx])
        view.end_edit(edit)

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)



class SublimeHaskellGoToAnyDeclaration(sublime_plugin.WindowCommand):
    def run(self):
        self.files = []
        self.declarations = []

        with autocompletion.database.files_lock:
            for f, m in autocompletion.database.files.items():
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
        SublimeHaskellAutocomplete.inspector.mark_all_files(self.window)



class SublimeHaskellSymbolInfoCommand(sublime_plugin.TextCommand):
    """
    Show information about selected symbol
    
    """
    def run(self, edit):
        word_region = self.view.word(self.view.sel()[0])
        preline = get_line_contents_before_region(self.view, word_region)
        (module_word, ident) = get_qualified_symbol(preline)

        current_file_name = self.view.file_name()

        candidates = []

        with autocompletion.database.symbols_lock:
            if ident not in autocompletion.database.symbols:
                show_status_message('Symbol {0} not found'.format(ident), False)
                return

            decls = autocompletion.database.symbols[ident]

            modules_dict = symbols.declarations_modules(decls, lambda ms: symbols.get_visible_module(ms, current_file_name)).values()

            with autocompletion.database.files_lock:
                if current_file_name in autocompletion.database.files:
                    cur_info = autocompletion.database.files[current_file_name]

                    if not module_word:
                        # this module declaration
                        candidates.extend([m.declarations[ident] for m in modules_dict if symbols.is_this_module(cur_info, m) and ident in m.declarations])
                    if not candidates:
                        # declarations from imported modules
                        candidates.extend([m.declarations[ident] for m in modules_dict if symbols.is_imported_module(cur_info, m, module_word) and ident in m.declarations])
                    if not candidates:
                        # show all possible candidates
                        candidates.extend([m.declarations[ident] for m in modules_dict if ident in m.declarations])

                # No info about imports for this file, just add all declarations
                else:
                    candidates.extend([m.declarations[ident] for m in modules_dict if ident in m.declarations])

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

    def show_symbol_info(self, decl):
        output_view = self.view.window().get_output_panel('sublime_haskell_symbol_info')
        output_view.set_read_only(False)

        if decl.what == 'declaration' and not decl.location:
            # Try to load detailed info
            decl_detailed = ghci_info_symbol(decl.module.name, decl.name)
            if decl_detailed:
                docs_info = haskell_docs(decl.module.name, decl.name)
                if docs_info:
                    decl_detailed.docs = docs_info

                autocompletion.database.add_declaration(decl_detailed, decl.module)
                decl = decl_detailed

        # TODO: Move to separate command for Sublime Text 3
        edit = output_view.begin_edit()
        output_view.insert(edit, output_view.size(), decl.detailed())
        output_view.end_edit(edit)

        output_view.sel().clear()
        output_view.set_read_only(True)

        self.view.window().run_command('show_panel', {
            'panel': 'output.' + 'sublime_haskell_symbol_info' })



class SublimeHaskellGoToDeclaration(sublime_plugin.TextCommand):
    def run(self, edit):
        word_region = self.view.word(self.view.sel()[0])
        preline = get_line_contents_before_region(self.view, word_region)
        (module_word, ident) = get_qualified_symbol(preline)

        full_name = '.'.join([module_word, ident]) if module_word else ident

        current_file_name = self.view.file_name()
        current_project = get_cabal_project_dir_of_file(current_file_name)

        module_candidates = []
        candidates = []

        with autocompletion.database.symbols_lock:
            if ident not in autocompletion.database.symbols:
                show_status_message('Declaration for {0} not found'.format(ident), False)
                return

            decls = autocompletion.database.symbols[ident]

            modules_dict = symbols.flatten(symbols.declarations_modules(decls, lambda ms: filter(symbols.is_by_sources, ms)).values())

            with autocompletion.database.files_lock:
                if current_file_name in autocompletion.database.files:
                    cur_info = autocompletion.database.files[current_file_name]

                    if not module_word:
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
                        candidates.extend([m.declarations[ident] for m in modules_dict if ident in declarations])

                # No info about imports for this file, just add all declarations
                else:
                    candidates.extend([m.declarations[ident] for m in modules_dict if ident in declarations])

        with autocompletion.database.modules_lock:
            if full_name in autocompletion.database.modules:
                modules = filter(symbols.is_by_sources, autocompletion.database.modules[full_name])

                # Find module in this project
                module_candidates.extend([m for m in modules if symbols.is_within_project(m, current_project)])
                if not module_candidates:
                    # Modules from other projects
                    module_candidates.extend(modules)

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

    def run(self):
        self.init_ghcmod_completions()

        # Load general info about all standard modules
        # with status_message('Updating standard modules'):
        #     for m in autocompletion.module_completions.copy():
        #         self._load_standard_module(m)

        while True:
            load_modules = []
            with self.modules_lock:
                load_modules = self.modules_to_load
                self.modules_to_load = []

            if len(load_modules) > 0:
                with status_message('Updating standard modules extended info') as s:
                    try:
                        for m in load_modules:            
                            self._load_standard_module(m)
                        # for m in load_modules:
                        #     self._load_standard_module_detailed(m)
                    except:
                        s.fail()
                        continue

    def load_module_info(self, module_name):
        with self.modules_lock:
            self.modules_to_load.append(module_name)

    # Gets available LANGUAGE options and import modules from ghc-mod
    def init_ghcmod_completions(self):

        if not get_setting_async('enable_ghc_mod'):
            return

        with status_message('Updating ghc_mod completions'):
            # Init LANGUAGE completions
            autocompletion.language_completions = call_ghcmod_and_wait(['lang']).splitlines()
            # Init import module completion
            autocompletion.module_completions = set(call_ghcmod_and_wait(['list']).splitlines())

    def _load_standard_module(self, module_name):
        if module_name not in autocompletion.database.get_cabal_modules():
            try:
                module_contents = call_ghcmod_and_wait(['browse', module_name]).splitlines()
                m = symbols.Module(module_name, cabal = symbols.current_cabal())
                for name in module_contents:
                    m.add_declaration(symbols.Declaration(name))

                autocompletion.database.add_module(m)

            except Exception as e:
                log('Inspecting in-cabal module {0} failed: {1}'.format(module_name, e))

    def _load_standard_module_detailed(self, module_name):
        if module_name in autocompletion.database.get_cabal_modules():
            try:
                msg = 'Loading info for {0}'.format(module_name)
                begin_time = time.clock()
                log('loading detailed info for standard module {0}'.format(module_name))

                with status_message(msg):
                    in_module = autocompletion.database.get_cabal_modules()[module_name]
                    decls = in_module.declarations.keys()
                    for decl in decls:
                        # Need to load extended info only if it wasn't loaded before
                        if in_module.declarations[decl].what == 'declaration':
                            decl_symbol = ghci_info_symbol(module_name, decl)
                            if decl_symbol:
                                decl_symbol.docs = haskell_docs(module_name, decl)
                                autocompletion.database.add_declaration(decl_symbol, in_module)

                end_time = time.clock()
                log('loaded detailed info for standard module {0} within {1} seconds'.format(module_name, end_time - begin_time))
            except Exception as e:
                log('Detailed inspecting in-cabal module {0} failed: {1}'.format(module_name, e))


class InspectorAgent(threading.Thread):
    std_inspector = StandardInspectorAgent()
    std_inspector.start()

    def __init__(self):
        # Call the superclass constructor:
        super(InspectorAgent, self).__init__()
        # Make this thread daemonic so that it won't prevent the program
        # from exiting.
        self.daemon = True
        # Files that need to be re-inspected:
        self.dirty_files_lock = threading.Lock()
        self.dirty_files = []

    CABALMSG = 'Compiling Haskell CabalInspector'
    MODULEMSG = 'Compiling Haskell ModuleInspector'

    def run(self):
        # Compile the CabalInspector:
        # TODO: Where to compile it?
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
                '-o', MODULE_INSPECTOR_EXE_PATH,
                '-outputdir', MODULE_INSPECTOR_OBJ_DIR])

            if exit_code != 0:
                s.fail()
                error_msg = u"SublimeHaskell: Failed to compile ModuleInspector\n{0}".format(err)
                wait_for_window(lambda w: self.show_errors(w, error_msg))
                return

        # For first time, inspect all open folders and files
        wait_for_window(lambda w: self.mark_all_files(w))

        # TODO: If compilation failed, we can't proceed; handle this.
        # Periodically wake up and see if there is anything to inspect.
        while True:
            files_to_reinspect = []
            with self.dirty_files_lock:
                files_to_reinspect = self.dirty_files
                self.dirty_files = []
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
            time.sleep(AGENT_SLEEP_DURATION)

    def mark_all_files(self, window):
        folder_files = []
        for folder in window.folders():
            folder_files.extend(list_files_in_dir_recursively(folder))
        with self.dirty_files_lock:
            self.dirty_files.extend(folder_files)

    def show_errors(self, window, error_text):
        sublime.set_timeout(lambda: output_error(window, error_text), 0)

    def mark_file_dirty(self, filename):
        "Report that a file should be reinspected."
        with self.dirty_files_lock:
            self.dirty_files.append(filename)

    def _refresh_all_module_info(self, cabal_dir, index, count):
        "Rebuild module information for all files under the specified directory."
        begin_time = time.clock()
        log('reinspecting project ({0})'.format(cabal_dir))
        # Process all files within the Cabal project:
        # TODO: Only process files within the .cabal file's "src" directory.
        (project_name, cabal_file) = get_cabal_in_dir(cabal_dir)

        with status_message_process('Reinspecting ({0}/{1}) {2}'.format(index, count, project_name)) as s:

            # set project and read cabal
            if cabal_file and project_name:
                self._refresh_project_info(cabal_dir, project_name, cabal_file)

            files_in_dir = list_files_in_dir_recursively(cabal_dir)
            haskell_source_files = [x for x in files_in_dir if x.endswith('.hs') and ('dist/build/autogen' not in x)]
            for filename in haskell_source_files:
                self._refresh_module_info(filename)
            end_time = time.clock()
            log('total inspection time: {0} seconds'.format(end_time - begin_time))

    def _refresh_project_info(self, cabal_dir, project_name, cabal_file):
        exit_code, out, err = call_and_wait(
            [CABAL_INSPECTOR_EXE_PATH, cabal_file])

        if exit_code == 0:
            new_info = json.loads(out)

            if 'error' not in new_info:
                if 'executables' in new_info:
                    with autocompletion.projects_lock:
                        autocompletion.projects[project_name] = {
                            'dir': cabal_dir,
                            'cabal': os.path.basename(cabal_file),
                            'executables': new_info['executables'],
                        }

    def _refresh_module_info(self, filename):
        "Rebuild module information for the specified file."
        # TODO: Only do this within Haskell files in Cabal projects.
        # TODO: Skip this file if it hasn't changed since it was last inspected.
        # TODO: Currently the ModuleInspector only delivers top-level functions
        #       with hand-written type signatures. This code should make that clear.
        # If the file hasn't changed since it was last inspected, do nothing:
        if not filename.endswith('.hs'):
            return

        modification_time = os.stat(filename).st_mtime
        if CHECK_MTIME:
            inspection_time = self._get_inspection_time_of_file(filename)
            if modification_time <= inspection_time:
                return
        exit_code, stdout, stderr = call_and_wait(
            [MODULE_INSPECTOR_EXE_PATH, filename])
        # Update only when module is ok
        if exit_code == 0:
            new_info = json.loads(stdout)

            if 'error' not in new_info:
                # # Load standard modules
                if 'imports' in new_info:
                    for mi in new_info['imports']:
                        if 'importName' in mi:
                            InspectorAgent.std_inspector.load_module_info(mi['importName'])

                # # Remember when this info was collected.
                # new_info['inspectedAt'] = modification_time
                # # Dump the currently-known module info to disk:
                # formatted_json = json.dumps(autocompletion.info, indent=2)
                # with open(OUTPUT_PATH, 'w') as f:
                #     f.write(formatted_json)
                # with autocompletion.info_lock:
                #     autocompletion.info[filename] = new_info
                # autocompletion.module_completions.add(new_info['moduleName'])

                try:
                    def make_import(import_info):
                        import_name = import_info['importName']
                        ret = symbols.Import(import_name, import_info['qualified'], import_info['as'])
                        return (import_name, ret)

                    module_imports = dict(map(make_import, new_info['imports']))
                    import_list = new_info['exportList'] if ('exportList' in new_info and new_info['exportList'] is not None) else []
                    new_module = symbols.Module(new_info['moduleName'], import_list, module_imports, {}, filename)
                    for d in new_info['declarations']:
                        location = symbols.Location(filename, d['line'], d['column'])
                        if d['what'] == 'function':
                            new_module.add_declaration(symbols.Function(d['name'], d['type'], None, location))
                        elif d['what'] == 'type':
                            new_module.add_declaration(symbols.Type(d['name'], d['context'], d['args'], None, location))
                        elif d['what'] == 'data':
                            new_module.add_declaration(symbols.Data(d['name'], d['context'], d['args'], None, location))
                        elif d['what'] == 'class':
                            new_module.add_declaration(symbols.Class(d['name'], d['context'], d['args'], None, location))
                        else:
                            new_module.add_declaration(symbols.Declaration(d['name'], 'declaration', None, location))

                    autocompletion.database.add_file(filename, new_module)

                    for i in new_module.imports.values():
                        InspectorAgent.std_inspector.load_module_info(i.module)

                except Exception as e:
                    log('Inspecting file {0} failed: {1}'.format(filename, e))

    def _get_inspection_time_of_file(self, filename):
        """Return the time that a file was last inspected.`
        Return zero if it has never been inspected."""
        # TODO: Implement!
        return 0.0
        # with autocompletion.info_lock:
        #     try:
        #         return autocompletion.info[filename]['inspectedAt']
        #     except KeyError:
        #         return 0.0


def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files


class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    inspector = InspectorAgent()
    inspector.start()

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
            # TODO: Changed completion settings!
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
                import_completions = [(unicode(c),) * 2 for c in autocompletion.module_completions]

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
        filename = view.file_name()
        if filename:
            SublimeHaskellAutocomplete.inspector.mark_file_dirty(filename)

    def on_load(self, view):
        self.set_cabal_status(view)

    def on_activated(self, view):
        self.set_cabal_status(view)

    def on_post_save(self, view):
        filename = view.file_name()
        if filename:
            SublimeHaskellAutocomplete.inspector.mark_file_dirty(filename)

    def on_query_context(self, view, key, operator, operand, match_all):
        if key == 'auto_completion_popup':
            return get_setting('auto_completion_popup')
        elif key == 'is_haskell_source':
            return is_haskell_source(view)
        else:
            return False
