# -*- coding: UTF-8 -*-

import json
import os
import os.path
import re
import sublime
import sublime_plugin
import threading
import time
import sys
import webbrowser

try:
    import queue
except ImportError:
    import Queue as queue


if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import symbols
    import hdocs
    from ghci import ghci_info
    from hdevtools import start_hdevtools, stop_hdevtools
    from parseoutput import write_panel
    import hsdev
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.symbols as symbols
    import SublimeHaskell.hdocs as hdocs
    from SublimeHaskell.ghci import ghci_info
    from SublimeHaskell.hdevtools import start_hdevtools, stop_hdevtools
    from SublimeHaskell.parseoutput import write_panel
    import SublimeHaskell.hsdev as hsdev


# If true, files that have not changed will not be re-inspected.
CHECK_MTIME = True

HSDEV_CACHE_PATH = None
HSDEV_LOG = None

# The agent sleeps this long between inspections.
AGENT_SLEEP_TIMEOUT = 60.0

# Checks if we are in a LANGUAGE pragma.
LANGUAGE_RE = re.compile(r'.*{-#\s+LANGUAGE.*')
# Checks if we are in OPTIONS_GHC pragma
OPTIONS_GHC_RE = re.compile(r'.*{-#\s+OPTIONS_GHC.*')

WORD_RE = re.compile(r'^(?P<word>[\w\d\'\.]*)(?P<tail>.*)')

# Checks if we are in an import statement.
IMPORT_RE = re.compile(r'.*import(\s+qualified)?\s+')
IMPORT_RE_PREFIX = re.compile(r'^\s*import(\s+qualified)?\s+([\w\d\.]*)$')
IMPORT_QUALIFIED_POSSIBLE_RE = re.compile(r'.*import\s+(?P<qualifiedprefix>\S*)$')

# Checks if a word contains only alhanums, -, and _, and dot
NO_SPECIAL_CHARS_RE = re.compile(r'^(\w|[\-\.])*$')

# Get symbol qualified prefix and its name
SYMBOL_RE = re.compile(r'((?P<module>[A-Z][\w\d]*(\.[A-Z][\w\d\']*)*)\.)?((?P<identifier>(\w[\w\d\']*)?)|(?P<operator>[!#$%&*+\./<=>?@\\\^|\-~:]+))$')
# Get import name
IMPORT_MODULE_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)\b')
# SYMBOL_RE = re.compile(r'((?P<module>\w+(\.\w+)*)\.)?(?P<identifier>((\w*)|([]*)))$')
# Get symbol module scope and its name within import statement
IMPORT_SYMBOL_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)(\s+as\s+(?P<as>[A-Z][\w\d\']*))?\s*\(.*?((?P<identifier>([a-z][\w\d\']*)?)|(\((?P<operator>[!#$%&*+\.\/<=>?@\\\^|\-~:]*)))$')
# Export module
EXPORT_MODULE_RE = re.compile(r'\bmodule\s+[\w\d\.]*$')

def is_scanned_source(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)
    if file_shown_in_view is None:
        return False
    m = head_of(hsdev_client.module(file = file_shown_in_view))
    return m is not None

def is_in_project(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)
    if file_shown_in_view is None:
        return False
    m = head_of(hsdev_client.module(file = file_shown_in_view))
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

class QualifiedSymbol(object):
    def __init__(self, name = None, module = None, module_as = None, is_import_list = False, is_operator = False):
        self.name = name
        self.module = module
        self.module_as = module_as
        self.is_import_list = is_import_list
        self.is_operator = is_operator

    def qualified_name(self):
        if self.name is None:
            return self.module
        return '{0}.{1}'.format(self.module_as or self.module, self.name) if self.module else self.name

    def full_name(self):
        if self.name is None:
            return self.module
        return '{0}.{1}'.format(self.module, self.name) if self.module else self.name

    def is_module(self):
        return self.name is None and self.module is not None

def get_qualified_symbol(line):
    """
    Get module context of symbol and symbol itself
    Returns (module, as, name, is_import_list, is_operator), where module (or one of) can be None
    """
    res = IMPORT_SYMBOL_RE.search(line)
    if res:
        return QualifiedSymbol(
            name = next(i for i in [res.group('identifier'), res.group('operator')] if i is not None),
            module = res.group('module'),
            module_as = res.group('as'),
            is_import_list = True,
            is_operator = bool(res.group('operator')))
    res = IMPORT_MODULE_RE.search(line)
    if res:
        return QualifiedSymbol(module = res.group('module'))
    res = SYMBOL_RE.search(line)
    # res always match
    return QualifiedSymbol(
        module = res.group('module'),
        name = next(i for i in [res.group('identifier'), res.group('operator')] if i is not None),
        is_operator = bool(res.group('operator')))

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


# Gets available LANGUAGE options and import modules from ghc-mod
def get_ghcmod_language_pragmas():

    if get_setting_async('enable_hsdev'):
        return hsdev_client.ghcmod_lang()
    elif get_setting_async('enable_ghc_mod'):
        return call_ghcmod_and_wait(['lang']).splitlines()

    return []

def get_ghcmod_flags_pragmas():

    if get_setting_async('enable_hsdev'):
        return hsdev_client.ghcmod_flags()
    elif get_setting_async('enable_ghc_mod'):
        return call_ghcmod_and_wait(['flag']).splitlines()

    return []

# Background worker
class Worker(threading.Thread):
    def __init__(self):
        super(Worker, self).__init__()
        self.jobs = queue.Queue()

    def run(self):
        while True:
            name, fn, args, kwargs = self.jobs.get()
            try:
                fn(*args, **kwargs)
            except Exception as e:
                log('worker: job {0} fails with {1}'.format(name, e), log_debug)

    def async(self, name, fn, *args, **kwargs):
        self.jobs.put((name, fn, args, kwargs))

worker = None

def run_async(name, fn, *args, **kwargs):
    global worker
    if not worker:
        worker = Worker()
        worker.start()
    worker.async(name, fn, *args, **kwargs)



def sort_completions(comps):
    comps.sort(key = lambda k: k[0])

def sorted_completions(comps):
    return list(set(comps)) # unique + sort

def make_completions(suggestions):
    return sorted_completions([s.suggest() for s in (suggestions or [])])

def make_locations(comps):
    return sorted([[s.brief(), s.get_source_location()] for s in comps if s.has_source_location()], key = lambda k: k[0])

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

hsdev_inspector = None
hsdev_client = None
hsdev_client_back = None

# Set reinspect event
def dirty(fn):
    def wrapped(self, *args, **kwargs):
        if not hasattr(self, 'dirty_lock'):
            self.dirty_lock = threading.Lock()
        acquired = None
        if python3():
            acquired = self.dirty_lock.acquire(blocking = False)
        else:
            acquired = self.dirty_lock.acquire(False)
        try:
            return fn(self, *args, **kwargs)
        finally:
            if acquired:
                self.dirty_lock.release()
                self.reinspect_event.set()
    return wrapped

def use_inspect_modules(fn):
    def wrapped(self, *args, **kwargs):
        if get_setting_async('inspect_modules'):
            return fn(self, *args, **kwargs)
    return wrapped

def hsdev_connected():
    return hsdev_inspector.hsdev_connected()

# Return default value if hsdev is not enabled/connected
def use_hsdev(def_val = None):
    def wrap(fn):
        def wrapped(*args, **kwargs):
            if hsdev.hsdev_enabled() and hsdev_connected():
                return fn(*args, **kwargs)
            else:
                return def_val
        return wrapped
    return wrap

# Autocompletion data
class AutoCompletion(object):
    """Information for completion"""
    def __init__(self):
        self.language_pragmas = []
        self.flags_pragmas = []

        # cabal name => set of modules, where cabal name is 'cabal' for cabal or sandbox path for cabal-devs
        self.module_completions = LockedObject({})

        # keywords
        # TODO: keywords can't appear anywhere, we can suggest in right places
        self.keyword_completions = list(map(
            lambda k: (k + '\tkeyword', k),
            ['do', 'case', 'of', 'let', 'in', 'data', 'instance', 'type', 'newtype', 'where', 'deriving', 'import', 'module']))

        self.current_filename = None

        # filename ⇒ preloaded completions + None ⇒ all completions
        self.cache = LockedObject(CompletionCache())
        self.wide_completion = None

    def mark_wide_completion(self, view):
        self.wide_completion = view

    @use_hsdev([])
    def get_completions_async(self, file_name = None):
        def log_result(r):
            log('completions: {0}'.format(len(r)), log_trace)
            return r
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
            log('preparing completions for {0}'.format(file_name), log_debug)
            current_module = head_of(hsdev_client_back.module(file = file_name))
            if current_module:
                comps = make_completions(
                    hsdev_client_back.complete('', file_name, timeout = None))

                # Get imports names
                # Note, that if module imported with 'as', then it can be used only with its synonym instead of full name
                import_names.extend([('{0}\tmodule {1}'.format(i.import_as, i.module), i.import_as) for i in current_module.imports if i.import_as])
                import_names.extend([('{0}\tmodule'.format(i.module), i.module) for i in current_module.imports if not i.import_as])

                comps.extend(import_names)
                sort_completions(comps)

        with self.cache as cache_:
            cache_.files[file_name] = comps
            return log_result(cache_.files[file_name])

    def drop_completions_async(self, file_name = None):
        log('drop prepared completions')
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
            if view and is_haskell_source(view):
                filename = view.file_name()
                if filename:
                    self.get_completions_async(filename)

    @use_hsdev([])
    def get_completions(self, view, prefix, locations):
        "Get all the completions that apply to the current file."

        current_file_name = view.file_name()

        if not current_file_name:
            return []

        self.current_filename = current_file_name
        line_contents = get_line_contents(view, locations[0])
        qsymbol = get_qualified_symbol(line_contents)
        qualified_prefix = qsymbol.qualified_name()

        suggestions = []

        wide = self.wide_completion == view
        if wide: # Drop wide
            self.wide_completion = None

        if qsymbol.module:
            current_module = head_of(hsdev_client.module(file = current_file_name))
            current_project = None
            if current_module:
                current_project = current_module.location.project
                if qsymbol.is_import_list:
                    if current_project:
                        # Search for declarations of qsymbol.module within current project
                        q_module = head_of(hsdev_client.scope_modules(file = current_file_name, input = qsymbol.module, search_type = 'exact'))
                        if q_module.by_source():
                            proj_module = hsdev_client.resolve(file = q_module.location.filename, exports = True)
                            if proj_module:
                                suggestions = proj_module.declarations.values()
                        elif q_module.by_cabal():
                            cabal_module = head_of(hsdev_client.module(q_module.name, search_type = 'exact', package = q_module.location.package.name))
                            if cabal_module:
                                suggestions = cabal_module.declarations.values()
                else:
                    suggestions = hsdev_client.complete(qualified_prefix, current_file_name, wide = wide)
            return self.keyword_completions + make_completions(suggestions)
        else:
            with self.cache as cache_:
                if wide:
                    return self.keyword_completions + cache_.global_completions()
                else:
                    return self.keyword_completions + cache_.files.get(current_file_name, cache_.global_completions())

    @use_hsdev([])
    def completions_for_module(self, module, filename = None):
        """
        Returns completions for module
        """
        if not module:
            return []
        ms = []
        if filename:
            ms = hsdev_client.scope_modules(filename, input = module, search_type = 'exact')
        m = head_of(hsdev_client.module(
            input = module,
            search_type = 'exact',
            file = ms[0].location.filename if ms and ms[0].by_source() else None,
            db = ms[0].location.db if ms and ms[0].by_cabal() else None,
            package = ms[0].location.package.name if ms and ms[0].by_cabal() else None))
        if not m:
            return []
        return make_completions(m.declarations.values())

    def completions_for(self, module_name, filename = None):
        """
        Returns completions for module
        """
        return self.completions_for_module(module_name, filename)

    @use_hsdev([])
    def get_import_completions(self, view, prefix, locations):

        self.current_filename = view.file_name()
        line_contents = get_line_contents(view, locations[0])

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
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if get_setting('auto_complete_language_pragmas'):
            # TODO handle multiple selections
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                if not self.language_pragmas:
                    self.language_pragmas = get_ghcmod_language_pragmas()
                return [(to_unicode(c),) * 2 for c in self.language_pragmas]
            match_options = OPTIONS_GHC_RE.match(line_contents)
            if match_options:
                if not self.flags_pragmas:
                    self.flags_pragmas = get_ghcmod_flags_pragmas()
                return [(to_unicode(c),) * 2 for c in self.flags_pragmas]

        return []

    @use_hsdev([])
    def get_module_completions_for(self, qualified_prefix, modules = None, current_dir = None):
        def module_next_name(mname):
            """
            Returns next name for prefix
            pref = Control.Con, mname = Control.Concurrent.MVar, result = Concurrent.MVar
            """
            suffix = mname.split('.')[(len(qualified_prefix.split('.')) - 1):]
            # Sublime replaces full module name with suffix, if it contains no dots?
            return suffix[0]

        module_list = modules if modules else self.get_current_module_completions(current_dir = current_dir)
        return list(set((module_next_name(m) + '\tmodule', module_next_name(m)) for m in module_list if m.startswith(qualified_prefix)))

    @use_hsdev([])
    def get_current_module_completions(self, current_dir = None):
        """
        Get modules, that are in scope of file/project
        In case of file we just return 'scope modules' result
        In case of dir we look for a related project or sandbox:
            project - get dependent modules
            sandbox - get sandbox modules
        """
        if self.current_filename:
            return set([m.name for m in hsdev_client.scope_modules(self.current_filename)])
        elif current_dir:
            proj = hsdev_client.project(path = current_dir)
            if proj and 'path' in proj:
                return set([m.name for m in hsdev_client.list_modules(deps = proj['path'])])
            sbox = hsdev_client.sandbox(path = current_dir)
            if sbox and type(sbox) == dict and 'sandbox' in sbox:
                sbox = sbox.get('sandbox')
            if sbox:
                return set([m.name for m in hsdev_client.list_modules(sandbox = sbox)])
        else:
            return set([m.name for m in hsdev_client.list_modules(cabal = True)])

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

class SublimeHaskellComplete(SublimeHaskellTextCommand):
    """ Shows autocompletion popup """
    def run(self, edit, characters, wide = False):
        self.wide = wide
        if characters:
            for region in self.view.sel():
                self.view.insert(edit, region.end(), characters)

        # if can_complete_qualified_symbol(get_qualified_symbol_at_region(self.view, self.view.sel()[0])):
        self.view.run_command("hide_auto_complete")
        sublime.set_timeout(self.do_complete, 1)

    def do_complete(self):
        if self.wide:
            autocompletion.mark_wide_completion(self.view)
        self.view.run_command("auto_complete")



class SublimeHaskellBrowseDeclarations(SublimeHaskellTextCommand):
    """
    Show all available declarations in scope
    """
    def run(self, edit):
        self.decls = []
        self.current_file_name = self.view.file_name()
        self.status_msg = status_message_process('Browse declarations', priority = 3)
        self.status_msg.start()
        hsdev_client.scope(self.current_file_name, wait = False, on_response = self.on_resp, on_error = self.on_err)

    def on_resp(self, rs):
        self.decls = rs
        self.status_msg.stop()
        self.view.window().show_quick_panel([[decl.brief(), decl.docs.splitlines()[0] if decl.docs else ''] for decl in self.decls], self.on_done)

    def on_err(self, e):
        self.status_msg.fail()
        self.status_msg.stop()
        show_status_message('Browse declarations: {0}'.format(e))

    def on_done(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.view, self.decls[idx])



class SublimeHaskellFindDeclarations(SublimeHaskellWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.decls = hsdev_client.symbol(input = input, search_type = 'regex')
        if not self.decls:
            show_status_message("Nothing found for: {0}".format(input))
            return

        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(), str(decl.defined_module().location)] for decl in self.decls], self.on_select)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.window.active_view(), self.decls[idx])



class SublimeHaskellHayoo(SublimeHaskellWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.input = input
        self.status_msg = status_message_process("Hayoo '{0}'".format(self.input), priority = 3)
        self.status_msg.start()
        hsdev_client.hayoo(self.input, page = 0, pages = 5, on_response = self.on_resp, on_error = self.on_err, wait = False)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.window.active_view(), self.decls[idx])

    def on_err(self, e):
        self.status_msg.fail()
        self.status_msg.stop()
        show_status_message("Hayoo '{0}': {1}".format(self.input, e))

    def on_resp(self, rs):
        self.status_msg.stop()
        self.decls = rs
        if not self.decls:
            show_status_message("Hayoo '{0}': not found".format(self.input))
            return
        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(), str(decl.defined_module().location)] for decl in self.decls], self.on_select)



class SublimeHaskellSearch(SublimeHaskellWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.input = input
        self.decls = []
        self.status_msg = status_message_process("Search '{0}'".format(self.input), priority = 3)
        self.status_msg.start()
        hsdev_client.symbol(input = self.input, search_type = 'infix', wait = False, on_response = self.on_symbol, on_error = self.on_err)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.window.active_view(), self.decls[idx])

    def on_err(self, e):
        self.status_msg.fail()
        self.status_msg.stop()
        show_status_message("Search '{0}': {1}".format(self.input, e))

    def on_symbol(self, rs):
        self.decls.extend(rs)
        hsdev_client.hayoo(self.input, page = 0, pages = 5, wait = False, on_response = self.on_resp, on_error = self.on_err)

    def on_resp(self, rs):
        self.status_msg.stop()
        self.decls.extend(rs)
        if not self.decls:
            show_status_message("Search '{0}' not found".format(self.input))
            return
        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(), str(decl.defined_module().location)] for decl in self.decls], self.on_select)



# General goto command
class SublimeHaskellGoTo(SublimeHaskellWindowCommand):
    def run(self, project = False):
        self.decls = []
        self.declarations = []
        decls = []

        self.view = self.window.active_view()
        self.current_filename = self.view.file_name()
        (self.line, self.column) = self.view.rowcol(self.view.sel()[0].a)

        if project:
            current_project = head_of(hsdev_client.module(file = self.current_filename)).location.project
            if not current_project:
                show_status_message('File {0} is not in project'.format(self.current_filename), False)
                return

            decls = self.sorted_decls_name(hsdev_client.symbol(project = current_project))
            self.declarations = [[decl.brief(True), decl.module.name] for decl in decls]
        else:
            decls = self.sorted_decls_pos(hsdev_client.symbol(file = self.current_filename, locals = True))
            self.declarations = [[(decl.position.column * ' ') + decl.brief(True)] for decl in decls]
        self.decls = decls[:]

        if not decls:
            return

        self.window.show_quick_panel(self.declarations, self.on_done, 0, self.closest_idx(decls), self.on_highlighted if not project else None)

    def qualified_decls(self, decls):
        for decl in decls:
            decl.make_qualified()
        return decls

    def sorted_decls_name(self, decls):
        return list(sorted(decls, key = lambda d: d.name))

    def sorted_decls_pos(self, decls):
        return list(sorted(decls, key = lambda d: (d.position.line, d.position.column)))

    def closest_idx(self, decls):
        fdecls = list(filter(
            lambda d: d[1].defined_module().location.filename == self.current_filename,
            enumerate(decls)))
        if not fdecls:
            return -1
        return min(fdecls, key = lambda d: abs(d[1].position.line - self.line))[0]

    def on_done(self, idx):
        if idx == -1:
            return
        self.open(self.decls[idx])

    def on_highlighted(self, idx):
        if idx == -1:
            return
        self.open(self.decls[idx], True)

    def open(self, decl, transient = False):
        view = self.window.open_file(decl.get_source_location(), sublime.ENCODED_POSITION | sublime.TRANSIENT if transient else sublime.ENCODED_POSITION)



class SublimeHaskellGoToModule(SublimeHaskellWindowCommand):
    def run(self):
        self.modules = hsdev_client.list_modules(source = True)
        self.window.show_quick_panel([[m.name if m.name != 'Main' else 'Main in {0}'.format(m.location.to_string()), m.location.to_string()] for m in self.modules], self.on_done, 0, 0, self.on_highlighted)

    def on_done(self, idx):
        if idx == -1:
            return
        self.window.open_file(self.modules[idx].location.to_string())

    def on_highlighted(self, idx):
        if idx == -1:
            return

        self.window.open_file(self.modules[idx].location.to_string(), sublime.TRANSIENT)


class SublimeHaskellGoToHackagePackage(SublimeHaskellTextCommand):
    def run(self, edit):
        pack = self.view.settings().get('package')
        if pack:
            webbrowser.open('http://hackage.haskell.org/package/{0}'.format(pack))

    def is_enabled(self):
        return self.view.settings().get('package') is not None

    def is_visible(self):
        return is_haskell_symbol_info(self.view)


class SublimeHaskellGoToHackageModule(SublimeHaskellTextCommand):
    def run(self, edit):
        if is_haskell_symbol_info(self.view):
            pack = self.view.settings().get('package')
            mod = self.view.settings().get('module')
            if pack and mod:
                webbrowser.open('http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(pack, mod.replace('.', '-')))
        else:
            qsymbol = get_qualified_symbol_at_region(self.view, self.view.sel()[0])

            ms = []
            if qsymbol.is_module(): # module
                scope = self.view.file_name()
                if scope:
                    ms = [m for m in hsdev_client.scope_modules(
                        scope, input = qsymbol.module, search_type = 'exact') if m.by_cabal()]
                else:
                    ms = [m for m in hsdev_client.list_modules(db = m.location.db) if m.name == qsymbol.module and m.by_cabal()]
            else: # symbol
                scope = self.view.file_name()
                if scope:
                    decls = hsdev_client.whois(
                        qsymbol.qualified_name(),
                        file = scope)
                    if not decls:
                        decls = hsdev_client.lookup(
                            qsymbol.full_name(),
                            file = scope)
                    if not decls:
                        decls = hsdev_client.symbol(
                            input = qsymbol.full_name(),
                            search_type = 'exact')
                    if not decls:
                        show_status_message('Module for symbol {0} not found'.format(qsymbol.full_name()))
                        return
                    ms = [decl.defined_module() for decl in decls]

            if len(ms) == 0:
                show_status_message('Module {0} not found'.format(module_name))
                return
            if len(ms) == 1:
                webbrowser.open('http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(ms[0].location.package.package_id(), ms[0].name.replace('.', '-')))
            else:
                self.candidates = ms[:]
                self.view.window().show_quick_panel([[m.name, m.location.package.package_id()] for m in self.candidates], self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        webbrowser.open('http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(self.candidates[idx].location.package.package_id(), self.candidates[idx].name.replace('.', '-')))

    def is_enabled(self):
        return (self.view.settings().get('package') is not None) or is_haskell_source(self.view) or is_haskell_repl(self.view)

    def is_visible(self):
        return is_haskell_symbol_info(self.view) or is_haskell_source(self.view) or is_haskell_repl(self.view)


class SublimeHaskellGoToAnyDeclaration(SublimeHaskellWindowCommand):
    def run(self):
        with autocompletion.cache as cache_:
            self.cache_ = cache_
            self.window.show_quick_panel(cache_.source_locs, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        self.window.open_file(self.cache_.source_locs[idx][1], sublime.ENCODED_POSITION)

class SublimeHaskellReinspectAll(SublimeHaskellWindowCommand):
    def run(self):
        if hsdev_inspector.hsdev_connected():
            log('reinspect all', log_trace)
            hsdev_inspector.start_inspect()
        else:
            show_status_message("inspector not connected", is_ok = False)

class SublimeHaskellScanContents(SublimeHaskellTextCommand):
    """
    Scan module contents
    """
    def run(self, edit, filename = None):
        self.current_file_name = filename or self.view.file_name()
        self.status_msg = status_message_process("Scanning {0}".format(self.current_file_name), priority = 3)
        self.status_msg.start()

        def on_resp(r):
            self.status_msg.stop()
            update_completions_async([self.current_file_name])

        def on_err(r):
            self.status_msg.fail()
            self.status_msg.stop()

        hsdev_client.scan(contents = {self.current_file_name: self.view.substr(sublime.Region(0, self.view.size()))}, on_response = on_resp, on_error = on_err)

class SublimeHaskellInferDocs(SublimeHaskellTextCommand):
    """
    Infer types and scan docs for current module
    """
    def run(self, edit, filename = None):
        self.current_file_name = filename or self.view.file_name()
        self.status_msg = status_message_process("Scanning docs for {0}".format(self.current_file_name), priority = 3)
        self.status_msg.start()

        def run_infer():
            self.status_msg = status_message_process("Inferring types for {0}".format(self.current_file_name), priority = 3)
            self.status_msg.start()

            def on_resp_(r):
                self.status_msg.stop()
            def on_err_(e):
                self.status_msg.fail()
                self.status_msg.stop()

            hsdev_client.infer(files = [self.current_file_name], on_response = on_resp_, on_error = on_err_)

        def on_resp(r):
            self.status_msg.stop()
            run_infer()

        def on_err(e):
            self.status_msg.fail()
            self.status_msg.stop()
            run_infer()

        hsdev_client.docs(files = [self.current_file_name], on_response = on_resp, on_error = on_err)

class SublimeHaskellSymbolInfoCommand(SublimeHaskellTextCommand):
    """
    Show information about selected symbol

    """
    def run(self, edit, filename = None, module_name = None, package_name = None, db = None, name = None, qname = None, no_browse = False):
        if qname:
            self.full_name = qname
            self.current_file_name = self.view.file_name()
            # Try whois it
            self.candidates = hsdev_client.whois(qname, file = self.current_file_name)
            if not self.candidates:
                if filename:
                    self.candidates = hsdev_client.symbol(name, search_type = 'exact', file = filename)
                elif module_name and package_name and db:
                    self.candidates = hsdev_client.symbol(
                        name,
                        search_type = 'exact',
                        module = module_name,
                        db = symbols.PackageDb.from_string(db) if db else None,
                        package = package_name)
        else:
            self.current_file_name = self.view.file_name()

            qsymbol = get_qualified_symbol(qname) if qname else get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            module_word = qsymbol.module
            ident = qsymbol.name

            if ident is None: # module
                if not no_browse:
                    self.view.window().run_command('sublime_haskell_browse_module', {
                        'module_name': module_word,
                        'scope': self.current_file_name })
                return

            if not module_word and not ident:
                show_status_message('No symbol selected', False)
                return

            self.whois_name = qsymbol.qualified_name()
            self.full_name = qsymbol.full_name()

            self.candidates = (hsdev_client.whois(self.whois_name, self.current_file_name) or [])[:1]

            if not self.candidates:
                self.candidates = hsdev_client.lookup(self.full_name, self.current_file_name)

            if not self.candidates:
                self.candidates = hsdev_client.symbol(input = self.full_name, search_type = 'exact')

        if not self.candidates:
            show_status_message('Symbol {0} not found'.format(self.full_name))
            return

        if len(self.candidates) == 1:
            self.show_symbol_info(self.candidates[0])
            return

        if not no_browse:
            self.view.window().show_quick_panel([[c.qualified_name(), c.defined_module().location.to_string()] for c in self.candidates], self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        self.show_symbol_info(self.candidates[idx])

    def on_import_selected(self, idx):
        if idx == 0: # Yes, select imported module
            sublime.set_timeout(
                lambda: self.view.window().show_quick_panel(['{0}.{1}'.format(i[0], i[1]) for i in self.candidates], self.on_candidate_selected), 0)

    def on_candidate_selected(self, idx):
        if idx == -1:
            return

        (module_name, ident_name) = self.candidates[idx]
        info = hsdev_client.whois('{0}.{1}'.format(module_name, ident_name), self.view.file_name())

        if info:
            self.show_symbol_info(info[0])
        else:
            show_status_message("Can't get info for {0}.{1}".format(module_name, ident_name), False)

    def show_symbol_info(self, decl):
        show_declaration_info_panel(self.view, decl)

    def is_visible(self):
        return is_haskell_source(self.view) or is_haskell_repl(self.view)

# Show symbol info for declaration via calling command
def show_declaration_info(view, decl):
    if decl.by_hayoo():
        show_declaration_info_panel(view, decl)
        return

    info = {}
    info['name'] = decl.name
    info['qname'] = decl.qualified_name()
    info['module_name'] = decl.module.name
    if decl.by_source():
        info['filename'] = decl.defined_module().location.filename
    if decl.by_cabal() and decl.defined_module().location.package.name:
        info['package_name'] = decl.module.location.package.name
        info['db'] = decl.module.location.db.to_string()

    sublime.set_timeout(lambda: view.run_command('sublime_haskell_symbol_info', info), 0)

def show_declaration_info_panel(view, decl):
    info = decl.detailed()
    if get_setting_async('unicode_symbol_info'):
        info = info.replace('->', '\u2192').replace('::', '\u2237')
    v = write_panel(view.window(), info, 'sublime_haskell_symbol_info', syntax = 'HaskellSymbolInfo')
    v.settings().erase('location')
    v.settings().erase('package')
    v.settings().erase('module')
    if decl.has_source_location():
        v.settings().set('location', decl.get_source_location())
    if decl.by_cabal():
        v.settings().set('package', decl.defined_module().location.package.package_id())
        v.settings().set('module', decl.defined_module().name)

toggle_symbol_info = False

class SublimeHaskellToggleSymbolInfoCommand(SublimeHaskellWindowCommand):
    def run(self):
        global toggle_symbol_info
        toggle_symbol_info = not toggle_symbol_info
        show_status_message('continuous symbol info: {0}'.format('on' if toggle_symbol_info else 'off'))

class SublimeHaskellContinuousSymbolInfo(sublime_plugin.EventListener):
    def on_selection_modified(self, view):
        if toggle_symbol_info:
            if is_haskell_source(view) and view.file_name():
                view.run_command('sublime_haskell_symbol_info', { 'no_browse': True })

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
            qsymbol = get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            self.full_name = qsymbol.qualified_name()

        if hsdev_client.whois(self.full_name, self.current_file_name):
            show_status_message('Symbol {0} already in scope'.format(self.full_name))
            return

        self.candidates = hsdev_client.lookup(self.full_name, self.current_file_name)

        if not self.candidates:
            show_status_message('Symbol {0} not found'.format(self.full_name))
            return

        if len(self.candidates) == 1:
            self.add_import(self.candidates[0].module.name)
            return

        self.view.window().show_quick_panel([[c.module.name] for c in self.candidates], self.on_done)

    def add_import(self, module_name):
        self.module_name = module_name
        contents = self.view.substr(sublime.Region(0, self.view.size()))
        contents_part = contents[0 : list(re.finditer('^import.*$', contents, re.MULTILINE))[-1].end()]
        call_and_wait_tool(['hsinspect'], 'hsinspect', contents_part, self.on_inspected, check_enabled = False)

    def on_inspected(self, result):
        cur_module = hsdev.parse_module(json.loads(result)['module']) if self.view.is_dirty() else head_of(hsdev_client.module(file = self.current_file_name))
        imports = sorted(cur_module.imports, key = lambda i: i.position.line)
        after = [i for i in imports if i.module > self.module_name]

        insert_line = 0
        insert_gap = False

        if len(after) > 0:
            # Insert before after[0]
            insert_line = after[0].position.line - 1
        elif len(imports) > 0:
            # Insert after all imports
            insert_line = imports[-1].position.line
        elif len(cur_module.declarations) > 0:
            # Insert before first declaration
            insert_line = min([d.position.line for d in cur_module.declarations.values()]) - 1
            insert_gap = True
        else:
            # Insert at the end of file
            insert_line = self.view.rowcol(self.view.size())[0]

        insert_text = 'import {0}\n'.format(self.module_name) + ('\n' if insert_gap else '')

        pt = self.view.text_point(insert_line, 0)
        self.view.insert(self.edit, pt, insert_text)

        show_status_message('Import {0} added'.format(self.module_name), True)

    def on_done(self, idx):
        if idx == -1:
            return
        self.view.run_command('sublime_haskell_insert_import_for_symbol', {
            'filename': self.current_file_name,
            'module_name': self.candidates[idx].module.name })

    def is_visible(self):
        return is_haskell_source(self.view)


class SublimeHaskellClearImports(SublimeHaskellTextCommand):
    def run(self, edit, filename = None):
        self.current_file_name = filename
        self.edit = edit

        if not self.current_file_name:
            self.current_file_name = self.view.file_name()

        cur_module = head_of(hsdev_client.module(file = self.current_file_name))
        if not cur_module:
            log("module not scanned")
            return

        imports = sorted(cur_module.imports, key = lambda i: i.position.line)

        cmd = ['hsclearimports', self.current_file_name, '--max-import-list', '16']
        (exit_code, cleared, err) = call_and_wait(cmd)
        if exit_code != 0:
            log('hsclearimports error: {0}'.format(err), log_error)
            return

        new_imports = cleared.splitlines()

        if len(imports) != len(new_imports):
            log('different number of imports: {0} and {1}'.format(len(imports), len(new_imports)), log_error)
            return

        log('replacing imports for {0}'.format(self.current_file_name), log_trace)
        erased = 0
        for i, ni in zip(imports, new_imports):
            pt = self.view.text_point(i.position.line - 1 - erased, 0)
            if ni.endswith('()'):
                self.view.erase(edit, self.view.full_line(pt))
                erased = erased + 1
            else:
                self.view.replace(edit, self.view.line(pt), ni)

class SublimeHaskellBrowseModule(SublimeHaskellWindowCommand):
    """
    Browse module symbols
    """
    def run(self, module_name = None, package_name = None, filename = None, db = None, scope = None):
        self.candidates = []
        self.current_file_name = self.window.active_view().file_name()

        m = None

        if filename:
            m = head_of(hsdev_client.module(file = filename))
            if not m:
                show_status_message('Module {0} not found'.format(filename))
                return

        elif module_name:
            ms = []
            if scope:
                ms = hsdev_client.scope_modules(scope, input = module_name, search_type = 'exact')
            else:
                if self.current_file_name:
                    ms = hsdev_client.scope_modules(self.current_file_name, input = module_name, search_type = 'exact')
                else:
                    ms = hsdev_client.list_modules(module = module_name, db = symbols.PackageDb.from_string(db) if db else None)

            if len(ms) == 0:
                show_status_message('Module {0} not found'.format(module_name))
                return
            if len(ms) == 1:
                if ms[0].by_source():
                    m = head_of(hsdev_client.module(module_name, search_type = 'exact', file = ms[0].location.filename))
                elif ms[0].by_cabal():
                    m = head_of(hsdev_client.module(
                        module_name,
                        search_type = 'exact',
                        db = ms[0].location.db,
                        package = ms[0].location.package.name))
                else:
                    m = head_of(hsdev_client.module(module_name, search_type = 'exact'))
            else:
                self.candidates.extend([(m, [m.name, m.location.to_string()]) for m in ms])

        else:
            if self.current_file_name:
                ms = hsdev_client.scope_modules(self.current_file_name)
            else:
                ms = hsdev_client.list_modules(db = symbols.PackageDb.from_string(db) if db else None)
            self.candidates.extend([(m, [m.name, m.location.to_string()]) for m in ms])

        if m:
            decls = list(m.declarations.values())
            self.candidates = sorted(decls, key = lambda d: d.brief())

            self.window.show_quick_panel([[decl.brief(), decl.docs.splitlines()[0] if decl.docs else ''] for decl in self.candidates], self.on_symbol_selected)
            return

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
        if m.by_cabal() and m.location.package.name:
            info['package_name'] = m.location.package.name
            info['db'] = m.location.db.to_string()

        sublime.set_timeout(lambda: self.window.run_command('sublime_haskell_browse_module', info), 0)

    def on_symbol_selected(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.window.active_view(), self.candidates[idx])



class SublimeHaskellGoToDeclaration(SublimeHaskellTextCommand):
    def run(self, edit):
        qsymbol = get_qualified_symbol_at_region(self.view, self.view.sel()[0])

        if is_haskell_symbol_info(self.view): # Go to within symbol info window
            loc = self.view.settings().get('location')
            if loc:
                self.view.window().open_file(loc, sublime.ENCODED_POSITION)
            else:
                show_status_message('Source location of {0} not found'.format(qsymbol.name), False)
            return

        whois_name = qsymbol.qualified_name()
        full_name = qsymbol.full_name()

        current_file_name = self.view.file_name()
        current_project = get_cabal_project_dir_of_file(current_file_name)

        candidates = []
        module_candidates = []
        if not qsymbol.is_module():
            candidates = list(filter(lambda d: d.by_source(), hsdev_client.whois(whois_name, current_file_name)))

            if candidates and candidates[0].has_source_location():
                self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                return

            if candidates:
                cands = candidates[:]
                candidates = []
                for c in cands:
                    for i in c.imported:
                        candidates = [s for s in hsdev_client.symbol(input = c.name, search_type = 'exact', source = True) if s.module.name == i.module]
                        if candidates and candidates[0].has_source_location():
                            self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                            return

            candidates = hsdev_client.symbol(input = qsymbol.name, search_type = 'exact', source = True)
        else:
            module_candidates = [m for m in hsdev_client.list_modules(source = True, module = full_name) if m.name == full_name]

        if not candidates and not module_candidates:
            show_status_message('Declaration {0} not found'.format(qsymbol.name), False)
            return

        candidates_len = len(candidates) if candidates is not None else 0
        module_candidates_len = len(module_candidates) if module_candidates is not None else 0

        if candidates_len + module_candidates_len == 1:
            if candidates_len == 1:
                self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                return
            if module_candidates_len == 1:
                self.view.window().open_file(module_candidates[0].location.filename)
                return

        # many candidates
        self.select_candidates = [([c.brief(), c.get_source_location()], True) for c in candidates] + [([m.name, m.location.filename], False) for m in module_candidates]
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

    def is_enabled(self):
        return is_haskell_source(self.view) or is_haskell_repl(self.view) or (is_haskell_symbol_info(self.view) and self.view.settings().get('location'))


class SublimeHaskellEvalReplaceCommand(SublimeHaskellTextCommand):
    def run(self, edit, results = []):
        for i, r in enumerate(results):
            if r is not None:
                self.view.replace(edit, self.view.sel()[i], str(r))
        for j in range(len(results), len(self.view.sel())):
            self.view.erase(edit, self.view.sel()[j])

    def is_enabled(self):
        return True


def ghc_eval_x(rs):
    # Drop 'fail' to be 'None' and unwrap strings
    # No idea how to call this function
    def process(i):
        if type(i) == dict:
            return None
        try:
            x = json.loads(i) # FIXME: Is it ok?
            if type(x) == str:
                return x
        except ValueError:
            return i
        return i
    return list(map(process, rs))

def ghc_eval_merge_results(l, r):
    # Prefer result in 'l', but if there's 'fail' - use result from 'r'
    return [x or y for x, y in zip(l, r)]

class SublimeHaskellEvalSelectionCommand(SublimeHaskellTextCommand):
    def run(self, edit):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.results = ghc_eval_x(hsdev_client.ghc_eval(self.args))

        self.view.run_command('sublime_haskell_eval_replace', {
            'results': self.results })

    def is_enabled(self):
        return True

    def is_visible(self):
        return True

class SublimeHaskellApplyToSelectionCommand(SublimeHaskellTextCommand):
    def run(self, edit):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.edit = edit
        self.view.window().show_input_panel('Function', '', self.on_done, None, self.on_cancel)

    def is_enabled(self):
        return True

    def is_visible(self):
        return True

    def on_done(self, f):
        self.results = ghc_eval_x(hsdev_client.ghc_eval(["({0}) ({1})".format(f, a) for a in self.args]))
        self.string_results = ghc_eval_x(hsdev_client.ghc_eval(["({0}) ({1})".format(f, json.dumps(a)) for a in self.args]))

        self.view.run_command('sublime_haskell_eval_replace', {
            'results': ghc_eval_merge_results(self.results, self.string_results) })

    def on_cancel(self):
        pass

class SublimeHaskellApplyToSelectionListCommand(SublimeHaskellTextCommand):
    def run(self, edit):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.edit = edit
        self.view.window().show_input_panel('Function', '', self.on_done, None, self.on_cancel)

    def is_enabled(self):
        return True

    def is_visible(self):
        return True

    def on_done(self, f):
        self.results = ghc_eval_x(hsdev_client.ghc_eval(['({0}) [{1}]'.format(f, ", ".join(self.args))]))
        self.string_results = ghc_eval_x(hsdev_client.ghc_eval(['({0}) [{1}]'.format(f, ", ".join([json.dumps(a) for a in self.args]))]))
        self.res = ghc_eval_merge_results(self.results, self.string_results)

        if self.res[0] is None:
            return
        else:
            result_list = json.loads(self.res[0])
            self.view.run_command('sublime_haskell_eval_replace', { 'results': result_list })

    def on_cancel(self):
        pass


class AutoFixState(object):
    def __init__(self, view = None, corrections = [], selected = 0, undo_history = [], redo_history = []):
        self.view = view
        self.corrections = corrections
        self.selected = selected
        self.undo_history = undo_history[:]
        self.redo_history = redo_history[:]

    def is_active(self):
        return self.view and self.corrections

    def set(self, view, corrections, selected = 0, undo_history = [], redo_history = []):
        if self.is_active():
            self.clear()
        self.view = view
        self.view.settings().set('autofix', True)
        self.view.set_read_only(True)
        self.corrections = corrections
        self.selected = selected
        self.undo_history = undo_history[:]
        self.redo_history = redo_history[:]

    def clear(self):
        if self.view:
            self.view.set_read_only(False)
            self.view.settings().erase('autofix')
            self.unmark()
            self.view = None
            self.corrections.clear()
            self.undo_history.clear()
            self.redo_history.clear()
            self.selected = 0

    def get_corrections(self):
        corrs = self.corrections[:]
        cur = corrs.pop(self.selected)
        return (cur, corrs)

    def current_correction(self):
        return self.corrections[self.selected]

    def mark(self):
        (cur, corrs) = self.get_corrections()

        self.view.add_regions('autofix', [corr.to_region(self.view) for corr in corrs], 'warning', 'dot', sublime.DRAW_OUTLINED)
        rgns = [cur.to_region(self.view)]
        self.view.add_regions('autofix_current', rgns, 'warning', 'dot')
        self.view.show(sublime.Region(rgns[0].a, rgns[-1].b))
        write_panel(self.view.window(), 'Press {0}\n\n{1}'.format(self.keys(), self.message(cur)), 'sublime_haskell_auto_fix', syntax = 'HaskellAutoFix')

    def keys(self):
        return u'↑ ↓ ↵ ctrl+z ctrl+y esc'

    def message(self, cur):
        if cur.corrector.contents:
            return u'\u2014 {0}\n  Why not:\n\n{1}'.format(cur.message, cur.corrector.contents)
        return u'\u2014 {0}'.format(cur.message)

    def unmark(self):
        self.view.erase_regions('autofix')
        self.view.erase_regions('autofix_current')
        w = self.view.window()
        if w:
            w.run_command('hide_panel', {'panel': 'output.' + 'sublime_haskell_auto_fix'})

    def count(self):
        return len(self.corrections)

    def set_selected(self, i):
        if i < 0 or i >= len(self.corrections):
            log('AutoFixState.set_selected({0}): out of bound'.format(i), log_error)
            return
        self.selected = i
        self.mark()

    def fix_current(self):
        self.undo_history.append((self.corrections[:], self.selected))
        self.redo_history.clear()
        (cur, corrs) = self.get_corrections()
        self.corrections = hsdev_client.autofix_fix(hsdev.encode_corrections([cur]), rest = hsdev.encode_corrections(corrs), pure = True)
        if not self.corrections:
            self.selected = 0
            self.clear()
            return False
        if self.selected >= len(self.corrections):
            self.selected = 0
        self.mark()
        return True

    def has_undo(self):
        return len(self.undo_history) > 0

    def undo(self):
        if not self.has_undo():
            return False
        (corrs, sel) = self.undo_history.pop()
        self.redo_history.append((self.corrections[:], self.selected))
        self.corrections = corrs[:]
        self.selected = sel
        self.mark()
        return True

    def has_redo(self):
        return len(self.redo_history) > 0

    def redo(self):
        if not self.has_redo():
            return False
        (corrs, sel) = self.redo_history.pop()
        self.undo_history.append((self.corrections[:], self.selected))
        self.corrections = corrs[:]
        self.selected = sel
        self.mark()
        return True

autofix_state = AutoFixState()



class SublimeHaskellAutoFix(SublimeHaskellWindowCommand):
    def run(self):
        if self.window.active_view().file_name():
            def on_resp(msgs):
                self.messages = msgs
                self.status_msg.stop()
                if msgs is None:
                    return
                sublime.set_timeout(self.on_got_messages, 0)

            def on_err(err):
                self.status_msg.fail()
                self.status_msg.stop()
                show_status_message('Check & Lint: {0}'.format(err), False)

            self.status_msg = status_message_process('Autofix: ' + self.window.active_view().file_name(), priority = 3)
            self.status_msg.start()
            hsdev_client.check_lint(files = [self.window.active_view().file_name()], ghc = get_setting_async('ghc_opts'), wait = False, on_response = on_resp, on_error = on_err, timeout = 0)

    def on_got_messages(self):
        self.corrections = list(filter(lambda corr: os.path.samefile(corr.file, self.window.active_view().file_name()), hsdev_client.autofix_show(self.messages)))
        if self.corrections:
            autofix_state.set(self.window.active_view(), self.corrections)
            autofix_state.mark()

class SublimeHaskellAutoFixTextBase(SublimeHaskellTextCommand):
    def is_enabled(self):
        return autofix_state.is_active() and autofix_state.view == self.view

class SublimeHaskellAutoFixWindowBase(SublimeHaskellWindowCommand):
    def is_enabled(self):
        return autofix_state.is_active() and autofix_state.view == self.window.active_view()

class SublimeHaskellAutoFixPrevious(SublimeHaskellAutoFixWindowBase):
    def run(self):
        if autofix_state.selected > 0:
            autofix_state.set_selected(autofix_state.selected - 1)

class SublimeHaskellAutoFixNext(SublimeHaskellAutoFixWindowBase):
    def run(self):
        if autofix_state.selected + 1 < autofix_state.count():
            autofix_state.set_selected(autofix_state.selected + 1)

class SublimeHaskellAutoFixAll(SublimeHaskellAutoFixWindowBase):
    def run(self):
        autofix_state.unmark()
        self.window.active_view().run_command('sublime_haskell_auto_fix_fix_it', { 'all': True })
        autofix_state.clear()

class SublimeHaskellAutoFixFix(SublimeHaskellAutoFixWindowBase):
    def run(self):
        autofix_state.unmark()
        self.window.active_view().run_command('sublime_haskell_auto_fix_fix_it')
        autofix_state.fix_current()

class SublimeHaskellAutoFixFixIt(SublimeHaskellAutoFixTextBase):
    def run(self, edit, all = False):
        corrections = autofix_state.corrections[:] if all else [autofix_state.current_correction()]
        if corrections:
            corrs = sorted(
                [(correction.to_region(self.view), correction.corrector.contents) for correction in corrections],
                key = lambda c: c[0])

            self.view.set_read_only(False)
            rgns = [c[0] for c in corrs]
            # self.view.add_regions('autofix_fix', [c[0] for c in corrs], 'warning', 'dot', sublime.HIDDEN)

            for rgn, cts in corrs:
                if rgns:
                    self.view.add_regions('autofix_fix', rgns, 'warning', 'dot', sublime.HIDDEN)
                    self.view.replace(edit, rgns[0], cts)
                    rgns = self.view.get_regions('autofix_fix')
                    rgns.pop(0)

            # for i, (rgn, cts) in enumerate(corrs):
            #     rs = self.view.get_regions('autofix_fix')
            #     self.view.replace(edit, rs[i], cts)
            self.view.erase_regions('autofix_fix')
            self.view.set_read_only(True)

class SublimeHaskellAutoFixUndo(SublimeHaskellAutoFixWindowBase):
    def run(self):
        autofix_state.unmark()
        self.window.active_view().set_read_only(False)
        self.window.active_view().run_command('undo')
        self.window.active_view().set_read_only(True)
        autofix_state.undo()

    def is_enabled(self):
        return super(SublimeHaskellAutoFixUndo, self).is_enabled() and autofix_state.has_undo()

class SublimeHaskellAutoFixRedo(SublimeHaskellAutoFixWindowBase):
    def run(self):
        autofix_state.unmark()
        self.window.active_view().set_read_only(False)
        self.window.active_view().run_command('redo')
        self.window.active_view().set_read_only(True)
        autofix_state.redo()

    def is_enabled(self):
        return super(SublimeHaskellAutoFixRedo, self).is_enabled() and autofix_state.has_redo()

class SublimeHaskellAutoFixStop(SublimeHaskellAutoFixWindowBase):
    def run(self):
        autofix_state.clear()

class SublimeHaskellReplaceRegions(sublime_plugin.TextCommand):
    def run(self, edit, replaces):
        self.view.add_regions('sublime_haskell_replace_regions', [sublime.Region(start, end) for (start, end), text in replaces], 'warning', 'dot', sublime.HIDDEN)
        for i, (r, text) in enumerate(replaces):
            rs = self.view.get_regions('sublime_haskell_replace_regions')
            self.view.replace(edit, rs[i], text)
        self.view.erase_regions('sublime_haskell_replace_regions')

class hsdev_status(object):
    def __init__(self, status_message):
        self.status_message = status_message

    def __call__(self, msg):
        statuses = []
        for m in msg:
            p = m['progress']
            statuses.append('{0} ({1}/{2})'.format(m['name'], p['current'], p['total']) if p else m['name'])
        self.status_message.change_message('Inspecting {0}'.format(' / '.join(statuses)))

def update_completions_async(files = [], drop_all = False):
    if drop_all:
        run_async('drop all completions', autocompletion.drop_completions_async)
    else:
        for f in files:
            run_async('drop completions', autocompletion.drop_completions_async, f)
    run_async('init completions', autocompletion.init_completions_async)

class HsDevAgent(threading.Thread):
    def __init__(self):
        super(HsDevAgent, self).__init__()
        self.daemon = True
        self.cabal_to_load = LockedObject([])
        self.dirty_files = LockedObject([])
        self.dirty_paths = LockedObject([])
        self.hsdev_process = hsdev.HsDevProcess(cache = HSDEV_CACHE_PATH, log_file = HSDEV_LOG, log_config = get_setting_async('hsdev_log_config'))
        self.hsdev = hsdev.HsDev()
        self.hsdev_back = hsdev.HsDev()

        self.reinspect_event = threading.Event()

    def hsdev_connected(self):
        return self.hsdev.is_connected()

    def start_hsdev(self, start_server = True):
        min_ver = [0,1,7,0]
        max_ver = [0,1,9,0]

        hsdev_ver = hsdev.hsdev_version()
        if hsdev_ver is None:
            output_error_async(sublime.active_window(), "\n".join([
                "SublimeHaskell: hsdev executable couldn't be found!",
                "It's used in most features of SublimeHaskell",
                "Check if it's installed and in PATH",
                "If it's not installed, run 'cabal install hsdev' to install hsdev",
                "You may also want to adjust 'add_to_PATH' setting",
                "",
                "To supress this message and disable hsdev set 'enable_hsdev' to false"]))
        elif not hsdev.check_version(hsdev_ver, min_ver, max_ver):
            output_error_async(sublime.active_window(), "\n".join([
                "SublimeHaskell: hsdev version is incorrect: {0}".format(hsdev.show_version(hsdev_ver)),
                "Required version: >= {0} and < {1}".format(hsdev.show_version(min_ver), hsdev.show_version(max_ver)),
                "Update it by running 'cabal update' and 'cabal install hsdev'",
                "",
                "To supress this message and disable hsdev set 'enable_hsdev' to false"]))
        else:
            def start_():
                log('hsdev process started', log_trace)
                self.hsdev.connect_async()
                self.hsdev_back.connect_async()
            def exit_():
                log('hsdev process exited', log_trace)
                self.hsdev.close()
                self.hsdev_back.close()

            def connected_():
                log('hsdev agent: connected to hsdev', log_trace)
                self.hsdev.link()
            def back_connected_():
                log('hsdev agent: connected to hsdev', log_trace)
                self.start_inspect()

            self.hsdev.on_connected = connected_
            self.hsdev_back.on_connected = back_connected_

            self.hsdev_process.on_start = start_
            self.hsdev_process.on_exit = exit_

            self.hsdev_process.start()
            self.hsdev_process.create()

    def stop_hsdev(self):
        self.hsdev.close()
        self.hsdev_back.close()

    def on_hsdev_enabled(self, key, value):
        if key == 'enable_hsdev':
            if value:
                log("starting hsdev", log_info)
                self.hsdev_process.create()
            else:
                log("stopping hsdev", log_info)
                self.hsdev_process.stop()
                self.hsdev.close()
                self.hsdev_back.close()

    def on_inspect_modules_changed(self, key, value):
        if key == 'inspect_modules':
            if value:
                self.mark_all_files()

    def run(self):
        subscribe_setting('enable_hsdev', self.on_hsdev_enabled)
        subscribe_setting('inspect_modules', self.on_inspect_modules_changed)

        if hsdev.hsdev_enabled():
            self.start_hsdev()

        while True:
            if hsdev.hsdev_enabled() and not self.hsdev.ping():
                log('hsdev ping: no pong', log_warning)

            scan_paths = []
            with self.dirty_paths as dirty_paths:
                scan_paths = dirty_paths[:]
                dirty_paths[:] = []

            files_to_reinspect = []
            with self.dirty_files as dirty_files:
                files_to_reinspect = dirty_files[:]
                dirty_files[:] = []

            projects = []
            files = []

            if len(files_to_reinspect) > 0:
                projects = []
                files = []
                for f in files_to_reinspect:
                    d = get_cabal_project_dir_of_file(f)
                    if d is not None:
                        projects.append(d)
                    else:
                        files.append(f)

            projects = list(set(projects))
            files = list(set(files))

            try:
                self.inspect(paths = scan_paths, projects = projects, files = files)
            except Exception as e:
                log('HsDevAgent inspect exception: {0}'.format(e))

            load_cabal = []
            with self.cabal_to_load as cabal_to_load:
                load_cabal = cabal_to_load[:]
                cabal_to_load[:] = []

            for c in load_cabal:
                run_async('inspect cabal {0}'.format(c), self.inspect_cabal, c)

            if files_to_reinspect:
                update_completions_async(drop_all = True)
            self.reinspect_event.wait(AGENT_SLEEP_TIMEOUT)
            self.reinspect_event.clear()

    @dirty
    def force_inspect(self):
        self.reinspect_event.set()

    @dirty
    def start_inspect(self):
        self.mark_cabal()
        self.mark_all_files()

    @dirty
    @use_inspect_modules
    def mark_all_files(self):
        window = sublime.active_window()
        with self.dirty_files as dirty_files:
            dirty_files.extend(list(filter(lambda f: f and f.endswith('.hs'), [v.file_name() for v in window.views()])))
        with self.dirty_paths as dirty_paths:
            dirty_paths.extend(window.folders())

    @dirty
    @use_inspect_modules
    def mark_file_dirty(self, filename):
        if filename is None:
            return
        with self.dirty_files as dirty_files:
            dirty_files.append(filename)

    @dirty
    def mark_cabal(self, cabal_name = None):
        with self.cabal_to_load as cabal_to_load:
            cabal_to_load.append(cabal_name or 'cabal')

    @use_hsdev()
    def inspect_cabal(self, cabal = None):
        try:
            with status_message_process('Inspecting {0}'.format(cabal or 'cabal'), priority = 1) as s:
                self.hsdev_back.scan(cabal = (cabal == 'cabal'), sandboxes = [] if cabal == 'cabal' else [cabal], on_notify = hsdev_status(s), wait = True, docs = hdocs.hdocs_enabled())
        except Exception as e:
            log('loading standard modules info for {0} failed with {1}'.format(cabal or 'cabal', e), log_error)

    @use_hsdev()
    @use_inspect_modules
    def inspect(self, paths, projects, files):
        if paths or projects or files:
            try:
                with status_message_process('Inspecting', priority = 1) as s:
                    self.hsdev_back.scan(paths = paths, projects = projects, files = files, on_notify = hsdev_status(s), wait = True, ghc = get_setting_async('ghc_opts'), docs = hdocs.hdocs_enabled())
            except Exception as e:
                log('Inspection failed: {0}'.format(e), log_error)

    @use_hsdev()
    @use_inspect_modules
    def inspect_path(self, path):
        try:
            with status_message_process('Inspecting path {0}'.format(path), priority = 1) as s:
                self.hsdev_back.scan(paths = [path], on_notify = hsdev_status(s), wait = True, ghc = get_setting_async('ghc_opts'), docs = hdocs.hdocs_enabled())
        except Exception as e:
            log('Inspecting path {0} failed: {1}'.format(path, e), log_error)

    @use_hsdev()
    @use_inspect_modules
    def inspect_project(self, cabal_dir):
        (project_name, cabal_file) = get_cabal_in_dir(cabal_dir)

        try:
            with status_message_process('Inspecting project {0}'.format(project_name), priority = 1) as s:
                self.hsdev_back.scan(projects = [cabal_dir], on_notify = hsdev_status(s), wait = True, docs = hdocs.hdocs_enabled())
        except Exception as e:
            log('Inspecting project {0} failed: {1}'.format(cabal_dir, e), log_error)

    @use_hsdev()
    @use_inspect_modules
    def inspect_files(self, filenames):
        try:
            with status_message_process('Inspecting files', priority = 1) as s:
                self.hsdev_back.scan(files = filenames, on_notify = hsdev_status(s), wait = True, ghc = get_setting_async('ghc_opts'), docs = hdocs.hdocs_enabled())
        except Exception as e:
            log('Inspecting files failed: {0}'.format(e), log_error)

def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files



def is_inspected_source(view = None):
    return is_haskell_source(view) or is_cabal_source(view)

class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    def __init__(self):
        self.project_file_name = None

    def on_query_completions(self, view, prefix, locations):
        if not is_haskell_source(view):
            return []

        begin_time = time.clock()
        # Only suggest symbols if the current file is part of a Cabal project.

        completions = (autocompletion.get_import_completions(view, prefix, locations) +
            autocompletion.get_special_completions(view, prefix, locations))

        # Export list
        if 'meta.declaration.exports.haskell' in view.scope_name(view.sel()[0].a):
            line_contents = get_line_contents(view, locations[0])
            export_module = EXPORT_MODULE_RE.search(line_contents)
            if export_module:
                qsymbol = get_qualified_symbol_at_region(view, view.sel()[0])
                # TODO: Implement


        if not completions:
            completions = autocompletion.get_completions(view, prefix, locations)

        end_time = time.clock()
        log('time to get completions: {0} seconds'.format(end_time - begin_time), log_debug)

        # Don't put completions with special characters (?, !, ==, etc.)
        # into completion because that wipes all default Sublime completions:
        # See http://www.sublimetext.com/forum/viewtopic.php?t=8659
        # TODO: work around this
        # comp = [c for c in completions if NO_SPECIAL_CHARS_RE.match(c[0].split('\t')[0])]
        # if get_setting('inhibit_completions') and len(comp) != 0:
        #     return (comp, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        # return comp

        if get_setting('inhibit_completions') and len(completions) != 0:
            return (completions, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        return completions

    def set_cabal_status(self, view):
        filename = view.file_name()
        if filename:
            (cabal_dir, project_name) = get_cabal_project_dir_and_name_of_file(filename)
            if project_name:
                view.set_status('sublime_haskell_cabal', '{0}: {1}'.format('cabal', project_name)) # TODO: Set some useful status instead of this

    def on_activated_async(self, view):
        if is_haskell_source(view):
            filename = view.file_name()
            if filename:
                run_async('get completions for {0}'.format(filename), autocompletion.get_completions_async, filename)

    def on_new(self, view):
        start_inspector()

        self.set_cabal_status(view)
        if is_inspected_source(view):
            filename = view.file_name()
            if filename:
                hsdev_inspector.mark_file_dirty(filename)

    def on_load(self, view):
        start_inspector()

        self.set_cabal_status(view)
        if is_inspected_source(view):
            filename = view.file_name()
            if filename:
                hsdev_inspector.mark_file_dirty(filename)

    def on_activated(self, view):
        start_inspector()

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
                    log('project switched to {0}, reinspecting'.format(self.project_file_name))
                    if hsdev_inspector.hsdev_connected():
                        log('reinspect all', log_trace)
                        hsdev_client.remove_all()
                        hsdev_inspector.start_inspect()
                        hsdev_inspector.force_inspect()
                    else:
                        show_status_message("inspector not connected", is_ok = False)

    def on_post_save(self, view):
        if is_inspected_source(view):
            filename = view.file_name()
            if filename:
                hsdev_inspector.mark_file_dirty(filename)

    def on_query_context(self, view, key, operator, operand, match_all):
        if key == 'haskell_autofix':
            return view.settings().get('autofix')
        if key == 'auto_completion_popup':
            return get_setting('auto_completion_popup')
        elif key == 'haskell_source':
            return is_haskell_source(view)
        elif key == 'haskell_source_or_repl':
            return is_haskell_source(view) or is_haskell_repl(view)
        elif key == 'haskell_repl':
            return is_haskell_repl(view)
        elif key == 'haskell_symbol_info':
            return is_haskell_symbol_info(view)
        elif key == 'cabal_source':
            return is_cabal_source(view)
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

def start_inspector():
    global hsdev_inspector
    global hsdev_client
    global hsdev_client_back

    if hsdev_inspector is not None:
        return ()

    log('starting inspector', log_trace)

    hsdev_inspector = HsDevAgent()
    hsdev_client = hsdev_inspector.hsdev
    hsdev_client_back = hsdev_inspector.hsdev_back
    hsdev_inspector.start()

def plugin_loaded():
    global HSDEV_CACHE_PATH
    global HSDEV_LOG

    package_path = sublime_haskell_package_path()
    cache_path = sublime_haskell_cache_path()

    HSDEV_CACHE_PATH = os.path.join(cache_path, 'hsdev')
    HSDEV_LOG = os.path.join(HSDEV_CACHE_PATH, 'hsdev.log')

    # TODO: How to stop_hdevtools() in Sublime Text 2?
    start_hdevtools()

def plugin_unloaded():
    # Does this work properly on exit?
    stop_hdevtools()

if int(sublime.version()) < 3000:
    plugin_loaded()
