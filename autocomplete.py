import json
import os
import os.path
import re
import sublime
import sublime_plugin
import threading
import time
import sys

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import symbols
    import cache
    import util
    import hdocs
    from ghci import ghci_info
    from haskell_docs import haskell_docs
    from hdevtools import start_hdevtools, stop_hdevtools
    from parseoutput import write_panel
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
    from SublimeHaskell.parseoutput import write_panel
    import SublimeHaskell.hsdev as hsdev


# If true, files that have not changed will not be re-inspected.
CHECK_MTIME = True

INSPECTOR_ENABLED = False
INSPECTOR_RUNNING = False

HSDEV_CACHE_PATH = None

# The agent sleeps this long between inspections.
AGENT_SLEEP_TIMEOUT = 60.0

# Checks if we are in a LANGUAGE pragma.
LANGUAGE_RE = re.compile(r'.*{-#\s+LANGUAGE.*')

WORD_RE = re.compile(r'^(?P<word>[\w\d\'\.]*)(?P<tail>.*)')

# Checks if we are in an import statement.
IMPORT_RE = re.compile(r'.*import(\s+qualified)?\s+')
IMPORT_RE_PREFIX = re.compile(r'^\s*import(\s+qualified)?\s+(.*)$')
IMPORT_QUALIFIED_POSSIBLE_RE = re.compile(r'.*import\s+(?P<qualifiedprefix>\S*)$')

# Checks if a word contains only alhanums, -, and _, and dot
NO_SPECIAL_CHARS_RE = re.compile(r'^(\w|[\-\.])*$')

# Get symbol qualified prefix and its name
SYMBOL_RE = re.compile(r'((?P<module>[A-Z][\w\d]*(\.[A-Z][\w\d\']*)*)\.)?((?P<identifier>(\w[\w\d\']*)?)|(?P<operator>[!#$%&*+\./<=>?@\\\^|\-~:]+))$')
# Get import name
IMPORT_MODULE_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)\b')
# SYMBOL_RE = re.compile(r'((?P<module>\w+(\.\w+)*)\.)?(?P<identifier>((\w*)|([]*)))$')
# Get symbol module scope and its name within import statement
IMPORT_SYMBOL_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)(\s+as\s+(?P<as>[A-Z][\w\d\']*))?\s*\(.*?((?P<identifier>[a-z][\w\d\']*)|(?P<operator>[!#$%&*+\./<=>?@\\\^|\-~:]+))$')

def is_scanned_source(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)
    if file_shown_in_view is None:
        return False
    m = call_hsdev(hsdev.module, file = file_shown_in_view)
    return m is not None

def is_in_project(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)
    if file_shown_in_view is None:
        return False
    m = call_hsdev(hsdev.module, file = file_shown_in_view)
    if m is None:
        return False
    return m.location.project is not None

def get_line_contents(view, location):
    """
    Returns contents of line at the given location.
    """
    return view.substr(sublime.Region(view.line(location).a, location))

def logged(v, fmt = '{0}'):
    log(fmt.format(v), log_trace)
    return v

def get_line_contents_at_region(view, region):
    """
    Returns (before, at, after)
    """
    line_region = view.line(region)

    before = view.substr(sublime.Region(line_region.a, region.a))
    at = view.substr(region)
    after = view.substr(sublime.Region(region.b, line_region.b))
    return (before, at, after)

def get_line_contents_before_region(view, region):
    """
    Returns contents of line before the given region (including it).
    """
    (before, at, _) = get_line_contents_at_region(view, region)
    return before + at

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
    Returns (module, name, is_import_list, is_operator), where module (or one of) can be None
    """
    res = IMPORT_SYMBOL_RE.search(line)
    if res:
        return (res.group('module'), res.group('identifier') or res.group('operator'), True, bool(res.group('operator')))
    res = IMPORT_MODULE_RE.search(line)
    if res:
        return (res.group('module'), None, False, False)
    res = SYMBOL_RE.search(line)
    # res always match
    return (res.group('module'), res.group('identifier') or res.group('operator'), False, bool(res.group('operator')))

def get_qualified_symbol_at_region(view, region):
    """
    Get module context of symbol and symbol itself for line before (and with) word on region
    Returns (module, name), where module (or one of) can be None
    """
    (before, at, after) = get_line_contents_at_region(view, region)
    res = WORD_RE.match(after)
    if res:
        at = at + res.group('word')
    return get_qualified_symbol(before + at)

# Autocompletion data
class AutoCompletion(object):
    """Information for completion"""
    def __init__(self):
        self.language_completions = []
        # cabal name => set of modules, where cabal name is 'cabal' for cabal or sandbox path for cabal-devs
        self.module_completions = LockedObject({})

        # keywords
        # TODO: keywords can't appear anywhere, we can suggest in right places
        self.keyword_completions = map(
            lambda k: (k + '\t(keyword)', k),
            ['case', 'data', 'instance', 'type', 'where', 'deriving', 'import', 'module'])

        self.current_filename = None

    @hsdev.use_hsdev
    def get_completions(self, view, prefix, locations):
        "Get all the completions that apply to the current file."

        current_file_name = view.file_name()

        if not current_file_name:
            return []

        self.current_filename = current_file_name
        line_contents = get_line_contents(view, locations[0])
        (qualified_module, symbol_name, is_import_list, is_operator) = get_qualified_symbol(line_contents)
        qualified_prefix = '{0}.{1}'.format(qualified_module, symbol_name) if qualified_module else symbol_name

        suggestions = []
        if is_import_list:
            current_project = call_hsdev(hsdev.module, file = current_file_name).location.project
            if current_project:
                project_modules = [m.name for m in call_hsdev(hsdev.list_modules, project = current_project)]
                if qualified_module in project_modules:
                    suggestions = call_hsdev(hsdev.module, name = qualified_module, project = current_project).declarations.values()
            if not suggestions:
                suggestions = call_hsdev(hsdev.module, name = qualified_module, cabal = 'cabal').declarations.values()
        else:
            suggestions = call_hsdev(hsdev.complete, qualified_prefix, current_file_name) or []
            if not suggestions:
                suggestions = call_hsdev(hsdev.scope, current_file_name, global_scope = True, prefix = qualified_prefix) or []
            if not suggestions:
                suggestions = call_hsdev(hsdev.symbol, prefix = qualified_prefix) or []

        return list(set([s.suggest() for s in suggestions]))

    @hsdev.use_hsdev
    def completions_for_module(self, module, filename = None):
        """
        Returns completions for module
        """
        if not module:
            return []
        return map(lambda d: d.suggest(), call_hsdev(hsdev.module, name = module, file = filename).declarations.values())

    def completions_for(self, module_name, filename = None):
        """
        Returns completions for module
        """
        return self.completions_for_module(module_name, filename)

    @hsdev.use_hsdev
    def get_import_completions(self, view, prefix, locations):

        self.current_filename = view.file_name()
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if get_setting_async('auto_complete_language_pragmas'):
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                return [(to_unicode(c),) * 2 for c in self.language_completions]

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

    @hsdev.use_hsdev
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

    @hsdev.use_hsdev
    def get_current_module_completions(self):
        return set([m.name for m in call_hsdev(hsdev.scope_modules, file = self.current_filename, cabal = current_cabal())])


autocompletion = AutoCompletion()



def can_complete_qualified_symbol(info):
    """
    Helper function, returns whether sublime_haskell_complete can run for (module, symbol, is_import_list)
    """
    (module_name, symbol_name, is_import_list, is_operator) = info
    if not module_name:
        return False

    if is_import_list:
        return module_name in autocompletion.get_current_module_completions()
    else:
        return list(filter(lambda m: m.startswith(module_name), autocompletion.get_current_module_completions())) != []

class SublimeHaskellComplete(SublimeHaskellTextCommand):
    """ Shows autocompletion popup """
    def run(self, edit, characters):
        for region in self.view.sel():
            self.view.insert(edit, region.end(), characters)

        if can_complete_qualified_symbol(get_qualified_symbol_at_region(self.view, self.view.sel()[0])):
            self.view.run_command("hide_auto_complete")
            sublime.set_timeout(self.do_complete, 1)

    def do_complete(self):
        self.view.run_command("auto_complete")



class SublimeHaskellBrowseDeclarations(SublimeHaskellTextCommand):
    def run(self, edit):
        self.declarations = []
        decls = call_hsdev(hsdev.scope, self.view.file_name(), global_scope = True)

        for decl in decls:
            self.declarations.append([decl.brief(), decl.module.name])

        self.window.show_quick_panel(self.declarations, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        


class SublimeHaskellBrowseDeclarations(SublimeHaskellTextCommand):
    """
    Show all available declarations from current cabal and opened projects
    """
    def run(self, edit):
        self.decls = []
        self.declarations = []

        self.decls = call_hsdev(hsdev.scope, self.view.file_name(), global_scope = True)

        if not self.decls:
            show_status_message("Can't get scope for {0}".format(self.view.file_name()), False)

        for decl in self.decls:
            self.declarations.append([decl.module.name + ': ' + decl.brief()])

        self.view.window().show_quick_panel(self.declarations, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return

        decl = self.decls[idx]

        show_declaration_info(self.view, decl)



class SublimeHaskellFindDeclarations(SublimeHaskellWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.decls = call_hsdev(hsdev.symbol, find = input)
        if not self.decls:
            show_status_message("Nothing found for: {0}".format(input))
            return

        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(), str(decl.location)] for decl in self.decls], self.on_select)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return

        decl = self.decls[idx]

        show_declaration_info(self.window.active_view(), decl)



class SublimeHaskellHayoo(SublimeHaskellWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.decls = call_hsdev(hsdev.hayoo, query = input)
        if not self.decls:
            show_status_message("Nothing found for: {0}".format(input))
            return

        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(), str(decl.location)] for decl in self.decls], self.on_select)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return

        decl = self.decls[idx]
        
        show_declaration_info(self.window.active_view(), decl)



class SublimeHaskellSearch(SublimeHaskellWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.decls = []
        self.decls.extend(call_hsdev(hsdev.symbol, find = input) or [])
        self.decls.extend(call_hsdev(hsdev.hayoo, query = input) or [])
        if not self.decls:
            show_status_message("Nothing found for: {0}".format(input))
            return

        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(), str(decl.location)] for decl in self.decls], self.on_select)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return

        decl = self.decls[idx]

        show_declaration_info(self.window.active_view(), decl)



# General goto command
class SublimeHaskellGoTo(SublimeHaskellWindowCommand):
    def run(self, project = False):
        self.files = []
        self.declarations = []
        decls = []

        self.view = self.window.active_view()
        self.current_filename = self.view.file_name()
        (self.line, self.column) = self.view.rowcol(self.view.sel()[0].a)

        if project:
            current_project = call_hsdev(hsdev.module, file = self.current_filename).location.project
            if not current_project:
                show_status_message('File {0} is not in project'.format(self.current_filename), False)
                return

            decls = self.qualified_decls(self.sorted_decls(call_hsdev(hsdev.symbol, project = current_project)))
            self.declarations = [[decl.brief(), decl.location.position()] for decl in decls]
        else:
            decls = self.sorted_decls(call_hsdev(hsdev.symbol, file = self.current_filename, locals = True))
            self.declarations = [[(decl.location.column * ' ') + decl.brief()] for decl in decls]
        self.files = [[decl.location.filename, str(decl.location.line), str(decl.location.column)] for decl in decls]

        self.window.show_quick_panel(self.declarations, self.on_done, 0, self.closest_idx(decls), self.on_highlighted)

    def qualified_decls(self, decls):
        for decl in decls:
            decl.make_qualified()
        return decls

    def sorted_decls(self, decls):
        return list(sorted(decls, key = lambda d: (d.location.filename, d.location.line)))

    def closest_idx(self, decls):
        return min(
            filter(
                lambda d: d[1].location.filename == self.current_filename,
                enumerate(decls)),
            key = lambda d: abs(d[1].location.line - self.line))[0]

    def on_done(self, idx):
        if idx == -1:
            return
        self.window.open_file(':'.join(self.files[idx]), sublime.ENCODED_POSITION)

    def on_highlighted(self, idx):
        if idx == -1:
            return
        self.window.open_file(':'.join(self.files[idx]), sublime.ENCODED_POSITION | sublime.TRANSIENT)



class SublimeHaskellGoToAnyDeclaration(SublimeHaskellWindowCommand):
    def run(self):
        self.files = []
        self.declarations = []

        decls = call_hsdev(hsdev.symbol, source = True)

        for decl in decls:
            self.files.append([decl.location.filename, str(decl.location.line), str(decl.location.column)])
            self.declarations.append([decl.brief(), decl.location.position()])

        self.window.show_quick_panel(self.declarations, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        self.window.open_file(':'.join(self.files[idx]), sublime.ENCODED_POSITION)



class SublimeHaskellReinspectCabalCommand(SublimeHaskellWindowCommand):
    def run(self, old_cabal = None, new_cabal = None):
        if old_cabal is not None:
            call_hsdev(hsdev.remove, cabal = old_cabal)
        if new_cabal is not None:
            hsdev_inspector.mark_cabal(new_cabal)

class SublimeHaskellReinspectAll(SublimeHaskellWindowCommand):
    def run(self):
        global INSPECTOR_ENABLED

        if INSPECTOR_ENABLED:
            call_hsdev(hsdev.remove_all)
            hsdev_inspector.start_inspect()
        else:
            show_status_message("inspector_enabled setting is false", isok=False)


class SublimeHaskellSymbolInfoCommand(SublimeHaskellTextCommand):
    """
    Show information about selected symbol

    """
    def run(self, edit, filename = None, module_name = None, package_name = None, project_name = None, cabal = None, decl = None):
        if decl and (filename or module_name):
            self.full_name = decl
            self.current_file_name = filename
            self.candidates = call_hsdev(hsdev.symbol, 
                decl,
                project = project_name,
                file = self.current_file_name,
                module = module_name,
                package = package_name,
                cabal = cabal)
        else:
            self.current_file_name = self.view.file_name()

            (module_word, ident, _, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])

            if ident is None: # module
                self.view.window().run_command('sublime_haskell_browse_module', {
                    'module_name': module_word,
                    'scope': self.current_file_name })
                return

            if not module_word and not ident:
                show_status_message('No symbol selected', False)
                return

            self.full_name = '{0}.{1}'.format(module_word, ident) if module_word else ident

            self.candidates = call_hsdev(hsdev.whois, self.full_name, self.current_file_name)

            if not self.candidates:
                self.candidates = call_hsdev(hsdev.lookup, self.full_name, self.current_file_name)

            if not self.candidates:
                self.candidates = call_hsdev(hsdev.symbol, self.full_name)

        if not self.candidates:
            show_status_message('Symbol {0} not found'.format(self.full_name))
            return

        if len(self.candidates) == 1:
            self.show_symbol_info(self.candidates[0])
            return

        self.view.window().show_quick_panel([[c.qualified_name(), c.location_string()] for c in self.candidates], self.on_done)

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
        info = call_hsdev(hsdev.whois, '{0}.{1}'.format(module_name, ident_name), self.view.file_name())

        if info:
            self.show_symbol_info(info[0])
        else:
            show_status_message("Can't get info for {0}.{1}".format(module_name, ident_name), False)

    def show_symbol_info(self, decl):
        show_declaration_info_panel(self.view, decl)

# Show symbol info for declaration via calling command
def show_declaration_info(view, decl):
    if decl.by_hayoo():
        show_declaration_info_panel(view, decl)
        return

    info = {}
    info['decl'] = decl.name
    info['module_name'] = decl.module.name
    if decl.by_source():
        info['filename'] = decl.location.filename
        if decl.location.project:
            info['project_name'] = decl.location.project
    if decl.by_cabal() and decl.location.package.name:
        info['package_name'] = decl.location.package.name
        info['cabal'] = decl.location.cabal

    sublime.set_timeout(lambda: view.run_command('sublime_haskell_symbol_info', info), 0)

def show_declaration_info_panel(view, decl):
    write_panel(view.window(), decl.detailed(), 'sublime_haskell_symbol_info')

class SublimeHaskellInsertImportForSymbol(SublimeHaskellTextCommand):
    """
    Insert import for symbol
    """
    def run(self, edit, filename = None, decl = None, module_name = None):
        self.full_name = decl
        self.current_file_name = filename
        self.edit = edit

        if module_name is not None:
            self.add_import(module_name)
            return

        if not self.current_file_name:
            self.current_file_name = self.view.file_name()

        if not self.full_name:
            (module_word, ident, _, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            self.full_name = '{0}.{1}'.format(module_word, ident) if module_word else ident

        if call_hsdev(hsdev.whois, self.full_name, file = self.current_file_name):
            show_status_message('Symbol {0} already in scope'.format(self.full_name))
            return

        self.candidates = call_hsdev(hsdev.lookup, self.full_name, file = self.current_file_name)

        if not self.candidates:
            show_status_message('Symbol {0} not found'.format(self.full_name))
            return

        if len(self.candidates) == 1:
            self.add_import(self.candidates[0].module.name)
            return

        self.view.window().show_quick_panel([[c.qualified_name()] for c in self.candidates], self.on_done)

    def add_import(self, module_name):
        cur_module = call_hsdev(hsdev.module, file = self.current_file_name)
        imports = sorted(cur_module.imports, key = lambda i: i.location.line)
        after = [i for i in imports if i.module > module_name]

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

        insert_text = 'import {0}\n'.format(module_name) + ('\n' if insert_gap else '')

        pt = self.view.text_point(insert_line, 0)
        self.view.insert(self.edit, pt, insert_text)

        show_status_message('Import {0} added'.format(module_name), True)

    def on_done(self, idx):
        if idx == -1:
            return
        self.view.run_command('sublime_haskell_insert_import_for_symbol', {
            'filename': self.current_file_name,
            'module_name': self.candidates[idx].module.name })


class SublimeHaskellClearImports(SublimeHaskellTextCommand):
    def run(self, edit, filename = None):
        self.current_file_name = filename
        self.edit = edit

        if not self.current_file_name:
            self.current_file_name = self.view.file_name()

        (exit_code, cleared, err) = call_and_wait(['hsclearimports', self.current_file_name, '--max-import-list', '16'])
        if exit_code != 0:
            log('hsclearimports error: {0}'.format(err), log_error)
            return

        cur_module = call_hsdev(hsdev.module, file = self.current_file_name)
        imports = sorted(cur_module.imports, key = lambda i: i.location.line)
        new_imports = cleared.splitlines()

        if len(imports) != len(new_imports):
            log('different number of imports: {0} and {1}'.format(len(imports), len(new_imports)), log_error)
            return

        for i, ni in zip(imports, new_imports):
            pt = self.view.text_point(i.location.line - 1, 0)
            self.view.replace(edit, self.view.line(pt), ni)

class SublimeHaskellBrowseModule(SublimeHaskellWindowCommand):
    """
    Browse module symbols
    """
    def run(self, module_name = None, package_name = None, project_name = None, filename = None, cabal = None, scope = None):
        self.candidates = []

        m = None
        if filename:
            m = call_hsdev(
                hsdev.module,
                file = filename,
                package = package_name,
                project = project_name,
                cabal = cabal)
            if not m:
                show_status_message('Module {0} not found'.format(filename))
                return

        if module_name:
            ms = []
            if scope:
                ms = [m for m in call_hsdev(
                    hsdev.scope_modules,
                    file = scope,
                    cabal = cabal) if m.name == module_name]
            else:
                ms = [m for m in call_hsdev(
                    hsdev.list_modules,
                    package = package_name,
                    project = project_name,
                    cabal = cabal) if m.name == module_name]

            if len(ms) == 0:
                show_status_message('Module {0} not found'.format(module_name))
                return
            if len(ms) == 1:
                m = call_hsdev(
                    hsdev.module,
                    name = module_name,
                    package = package_name,
                    project = project_name,
                    cabal = cabal)
            else:
                self.candidates.extend([(m, [m.name, m.location_string()]) for m in ms])

        if m:
            decls = list(m.declarations.values())
            self.candidates = sorted(decls, key = lambda d: d.brief())

            self.window.show_quick_panel([[decl.brief(), decl.docs.splitlines()[0] if decl.docs else ''] for decl in self.candidates], self.on_symbol_selected)
            return

        if not self.candidates:
            self.candidates.extend([(m, [m.name, m.location_string()]) for m in call_hsdev(hsdev.list_modules, cabal = current_cabal())])
            self.candidates.extend([(m, [m.name, m.location_string()]) for m in call_hsdev(hsdev.list_modules, source = True)])

        self.candidates.sort(key = lambda c: c[1][0])

        self.window.show_quick_panel([c[1] for c in self.candidates], self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return

        m = self.candidates[idx][0]

        info = {}
        info['module_name'] = m.name
        if m.by_source():
            info['filename'] = m.location.filename
            if m.location.project:
                info['project_name'] = m.location.project
        if m.by_cabal() and m.location.package.name:
            info['package_name'] = m.location.package.name
            info['cabal'] = m.location.cabal

        sublime.set_timeout(lambda: self.window.run_command('sublime_haskell_browse_module', info), 0)

    def on_symbol_selected(self, idx):
        if idx == -1:
            return

        candidate = self.candidates[idx]

        show_declaration_info(self.window.active_view(), candidate)

class SublimeHaskellGoToDeclaration(SublimeHaskellTextCommand):
    def run(self, edit):
        (module_word, ident, _, _) = get_qualified_symbol_at_region(self.view, self.view.sel()[0])

        full_name = '.'.join([module_word, ident]) if module_word else ident

        current_file_name = self.view.file_name()
        current_project = get_cabal_project_dir_of_file(current_file_name)

        candidate = list(filter(lambda d: d.by_source(), call_hsdev(hsdev.whois, full_name, current_file_name)))

        if candidate and candidate[0].location and candidate[0].location.filename:
            self.view.window().open_file(candidate[0].location.position(), sublime.ENCODED_POSITION)
            return

        candidates = call_hsdev(hsdev.symbol, full_name, source = True)

        module_candidates = [m for m in call_hsdev(hsdev.list_modules, source = True) if m.name == full_name]

        if not candidates and not module_candidates:
            show_status_message('Declaration {0} not found'.format(ident), False)
            return

        if len(candidates) + len(module_candidates) == 1:
            if len(candidates) == 1:
                self.view.window().open_file(candidates[0].location.position(), sublime.ENCODED_POSITION)
                return
            if len(module_candidates) == 1:
                self.view.window().open_file(module_candidates[0].location.position(), sublime.ENCODED_POSITION)
                return

        # many candidates
        self.select_candidates = [([c.brief(), c.location.position()], True) for c in candidates] + [([m.name, m.location.filename], False) for m in module_candidates]
        self.view.window().show_quick_panel([c[0] for c in self.select_candidates], self.on_done, 0, 0, self.on_highlighted)

    def on_done(self, idx):
        if idx == -1:
            return

        selected = self.select_candidates[idx]
        if selected[1]:
            self.view.window().open_file(selected[0][1], sublime.ENCODED_POSITION)
        else:
            self.view.window().open_file(selected[0][1])

    def on_highlighted(self, idx):
        if idx == -1:
            return

        selected = self.select_candidates[idx]
        if selected[1]:
            self.view.window().open_file(selected[0][1], sublime.ENCODED_POSITION | sublime.TRANSIENT)
        else:
            self.view.window().open_file(selected[0][1], sublime.TRANSIENT)



hsdev_ipsnspector = None

class HsDevAgent(threading.Thread):
    def __init__(self):
        super(HsDevAgent, self).__init__()
        self.daemon = True
        self.cabal_to_load = LockedObject([])
        self.dirty_files = LockedObject([])
        self.hsdev_holder = hsdev.HsDevHolder(cache = HSDEV_CACHE_PATH)

        self.reinspect_event = threading.Event()

        self.hsdev_enabled_changed = False
        self.hsdev_enabled = False

    def start_hsdev(self):
        sublime.set_timeout(lambda: self.hsdev_holder.run_hsdev(), 0)
        return self.hsdev_holder.wait_hsdev()

    def stop_hsdev(self):
        self.hsdev_holder.call(hsdev.stop)

    def on_hsdev_enabled(self, key, value):
        self.hsdev_enabled_changed = True
        self.hsdev_enabled = value
        self.force_inspect()

    # Call hsdev
    def call(self, fn, *args, **kwargs):
        return self.hsdev_holder.call(fn, *args, **kwargs)

    def run(self):
        if hsdev.hsdev_enabled():
            self.start_hsdev()

        subscribe_setting('enable_hsdev', self.on_hsdev_enabled)

        self.start_inspect()

        while True:
            if self.hsdev_enabled_changed:
                self.hsdev_enabled_changed = False
                if self.hsdev_enabled:
                    log("starting hsdev", log_info)
                    self.start_hsdev()
                    self.call(hsdev.remove_all)
                    self.start_inspect()
                else:
                    log("stopping hsdev", log_info)
                    self.stop_hsdev()

            load_cabal = []
            with self.cabal_to_load as cabal_to_load:
                load_cabal = cabal_to_load[:]
                cabal_to_load[:] = []

            for c in load_cabal:
                try:
                    self.reinspect_cabal(c)
                except:
                    continue

            files_to_reinspect = []
            with self.dirty_files as dirty_files:
                files_to_reinspect = dirty_files[:]
                dirty_files[:] = []

            if len(files_to_reinspect) > 0:
                loaded_projects = [n['path'] for n in (call_hsdev(hsdev.list_projects) or [])]
                projects = []
                files = []
                for f in files_to_reinspect:
                    d = get_cabal_project_dir_of_file(f)
                    if d is not None and d not in loaded_projects:
                        projects.append(d)
                    else:
                        files.append(f)

                projects = list(set(projects))
                files = list(set(files))

                for i, p in enumerate(projects):
                    try:
                        self.reinspect_project(p, i + 1, len(projects))
                    except:
                        continue

                for f in files:
                    try:
                        self.reinspect_file(f)
                    except:
                        continue

            self.reinspect_event.wait(AGENT_SLEEP_TIMEOUT)
            self.reinspect_event.clear()

    def force_inspect(self):
        self.reinspect_event.set()

    def start_inspect(self):
        self.mark_cabal()
        wait_for_window(lambda w: self.mark_all_files(w))
        self.reinspect_event.set()

    def mark_all_files(self, window):
        folder_files = []
        for folder in window.folders():
            folder_files.extend(list_files_in_dir_recursively(folder))
        with self.dirty_files as dirty_files:
            dirty_files.extend([f for f in folder_files if f.endswith('.hs')])
        self.reinspect_event.set()

    def mark_file_dirty(self, filename):
        if filename is None:
            return
        with self.dirty_files as dirty_files:
            dirty_files.append(filename)
        self.reinspect_event.set()

    def mark_cabal(self, cabal_name = None):
        if not cabal_name:
            cabal_name = current_cabal()

        with self.cabal_to_load as cabal_to_load:
            cabal_to_load.append(cabal_name)
        self.reinspect_event.set()

    @hsdev.use_hsdev
    def reinspect_cabal(self, cabal = None):
        if not cabal:
            cabal = current_cabal()

        try:
            with status_message_process('Loading standard modules info for {0}'.format(cabal)) as s:
                begin_time = time.clock()
                log('loading standard modules info for {0}'.format(cabal), log_info)

                def cabal_status(msg):
                    if 'status' in msg:
                        s.change_message('Loading standard modules info for {0}: {1}'.format(cabal, msg['task']))

                call_hsdev(hsdev.scan, cabal = cabal, wait = True, on_status = cabal_status)

                end_time = time.clock()
                log('loading standard modules info for {0} within {1} seconds'.format(cabal, end_time - begin_time), log_debug)

        except Exception as e:
            log('loading standard modules info for {0} failed with {1}'.format(cabal, e), log_error)

    @hsdev.use_hsdev
    def reinspect_project(self, cabal_dir, index, count):
        begin_time = time.clock()
        log('reinspecting project ({0})'.format(cabal_dir), log_info)
        (project_name, cabal_file) = get_cabal_in_dir(cabal_dir)

        try:
            with status_message_process('Reinspecting ({0}/{1}) {2}'.format(index, count, project_name), priority = 1) as s:
                def file_scanned(msg):
                    if 'progress' in msg:
                        s.percentage_message(msg['progress']['current'], msg['progress']['total'])

                call_hsdev(hsdev.scan, projects = [cabal_dir], wait = True, on_status = file_scanned)

                end_time = time.clock()
                log('total inspection time: {0} seconds'.format(end_time - begin_time), log_debug)

        except Exception as e:
            log('Inspecting project {0} failed: {1}'.format(cabal_dir, e), log_error)

    @hsdev.use_hsdev
    def reinspect_file(self, filename):
        show_status_message('Reinspecting {0}'.format(filename))
        call_hsdev(hsdev.scan, files = [filename])

def call_hsdev(fn, *args, **kwargs):
    return hsdev_inspector.call(fn, *args, **kwargs)

def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files



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
            log('The inspector cannot be stopped as of now. You have to restart Sublime for that.', log_error)

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
            # std_inspector.load_cabal_info()
            pass

    def get_special_completions(self, view, prefix, locations):

        # Contents of the current line up to the cursor
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if get_setting('auto_complete_language_pragmas'):
            # TODO handle multiple selections
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                return [(to_unicode(c),) * 2 for c in autocompletion.language_completions]

        # Autocompletion for import statements
        if get_setting('auto_complete_imports'):
            match_import = IMPORT_RE.match(line_contents)
            if match_import:
                import_completions = [(to_unicode(c),) * 2 for c in autocompletion.get_current_module_completions()]

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
        log('time to get completions: {0} seconds'.format(end_time - begin_time), log_debug)
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
            if filename and INSPECTOR_ENABLED:
                hsdev_inspector.mark_file_dirty(filename)

    def on_load(self, view):
        global INSPECTOR_ENABLED

        self.set_cabal_status(view)
        if is_haskell_source(view):
            filename = view.file_name()
            if filename and INSPECTOR_ENABLED:
                hsdev_inspector.mark_file_dirty(filename)

    def on_activated(self, view):
        self.set_cabal_status(view)

    def on_post_save(self, view):
        global INSPECTOR_ENABLED

        if is_haskell_source(view):
            filename = view.file_name()
            if filename and INSPECTOR_ENABLED:
                hsdev_inspector.mark_file_dirty(filename)

    def on_query_context(self, view, key, operator, operand, match_all):
        if key == 'auto_completion_popup':
            return get_setting('auto_completion_popup')
        elif key == 'haskell_source':
            return is_haskell_source(view)
        elif key == 'scanned_source':
            return is_scanned_source(view)
        elif key == 'in_project':
            return is_in_project(view)
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

hsdev_inspector = None

def start_inspector():
    global INSPECTOR_RUNNING

    if INSPECTOR_RUNNING:
        raise Exception('SublimeHaskell: ModuleInspector is already running!')

    log('starting inspector', log_trace)

    global hsdev_inspector
    hsdev_inspector = HsDevAgent()
    hsdev_inspector.start()

    INSPECTOR_RUNNING = True

def plugin_loaded():
    global OUTPUT_PATH
    global HSDEV_CACHE_PATH
    global INSPECTOR_ENABLED
    global INSPECTOR_RUNNING

    package_path = sublime_haskell_package_path()
    cache_path = sublime_haskell_cache_path()

    HSDEV_CACHE_PATH = os.path.join(cache_path, 'hsdev')
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
