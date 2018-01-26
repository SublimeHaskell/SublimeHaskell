import functools
import json
import os.path
import webbrowser

import sublime
import sublime_plugin

# import SublimeHaskell.autocomplete as autocomplete
import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.hsdev.result_parse as HsDevResultParse
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.symbols as symbols

# Extract the filename, line, column from symbol info
SYMBOL_FILE_REGEX = r'^Defined at: (.*):(\d+):(\d+)$'


def show_declaration_info_panel(view, decl):
    info = decl.detailed()
    panel = Common.output_panel(view.window(), info, 'sublime_haskell_symbol_info_panel', syntax='HaskellSymbolInfo')
    panel.settings().set('result_file_regex', SYMBOL_FILE_REGEX)
    panel.settings().erase('location')
    panel.settings().erase('package')
    panel.settings().erase('module')
    if decl.has_source_location():
        panel.settings().set('location', decl.get_source_location())
    if decl.by_cabal():
        panel.settings().set('package', decl.defined_module().location.package.package_id())
        panel.settings().set('module', decl.defined_module().name)


# Show symbol info for declaration via calling command
def show_declaration_info(view, decl):
    if not decl.by_hayoo():
        info = {}
        info['name'] = decl.name
        info['qname'] = decl.qualified_name()
        if decl.module:
            info['module_name'] = decl.module.name
            if decl.by_source():
                info['filename'] = decl.defined_module().location.filename
            if decl.by_cabal() and decl.defined_module().location.package.name:
                info['package_name'] = decl.module.location.package.name
                info['db'] = decl.module.location.db.to_string()

        view.run_command('sublime_haskell_symbol_info', info)
    else:
        show_declaration_info_panel(view, decl)


class SublimeHaskellComplete(CommandWin.SublimeHaskellTextCommand):
    """ Show autocompletion popup """

    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, edit, **kwargs):
        characters = kwargs.get('characters')
        wide = kwargs.get('wide') or False
        view_settings = self.view.settings()
        view_settings.set('subhask_wide_completion', wide)
        if characters:
            for region in self.view.sel():
                self.view.insert(edit, region.end(), characters)

        # if can_complete_qualified_symbol(Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])):
        self.view.run_command("hide_auto_complete")
        self.view.run_command("auto_complete")


class SublimeHaskellBrowseDeclarations(CommandWin.BackendWindowCommand):
    """
    Show all available declarations in scope
    """
    def __init__(self, win):
        super().__init__(win)
        self.view = win.active_view()
        self.decls = []
        self.current_file_name = self.view.file_name()

    def run(self, **_kwargs):
        self.decls = BackendManager.active_backend().scope(self.current_file_name, on_error=self.on_err)
        brief_decls = [[decl.brief(use_unicode=True),
                        decl.docs.splitlines()[0] if decl.docs else ''] for decl in self.decls]
        # pprint.pprint(brief_decls)
        self.window.show_quick_panel(brief_decls, self.on_done)

    def on_err(self, err, _details):
        Common.sublime_status_message('Browse declarations: {0}'.format(err))

    def on_done(self, idx):
        if idx >= 0:
            show_declaration_info(self.view, self.decls[idx])


class SublimeHaskellFindDeclarations(CommandWin.BackendWindowCommand):
    def __init__(self, win):
        super().__init__(win)
        self.decls = []

    def run(self, **_kwargs):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, sym):
        self.decls = BackendManager.active_backend().symbol(lookup=sym, search_type='regex')
        self.decls.sort(key=lambda d: d.module.name)
        if self.decls:
            module_decls = [['{0}: {1}'.format(decl.module.name, decl.brief(use_unicode=True)),
                             str(decl.defined_module().location)] for decl in self.decls]
            self.window.show_quick_panel(module_decls, self.on_select)
        else:
            Common.sublime_status_message("Nothing found for: {0}".format(sym))

    def on_change(self, _sym):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx >= 0:
            show_declaration_info(self.window.active_view(), self.decls[idx])


class SublimeHaskellHayoo(CommandWin.BackendWindowCommand):
    def __init__(self, win):
        super().__init__(win)
        self.decls = []
        self.search_str = ''
        self.status_msg = None

    def run(self, **_kwargs):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, search_str):
        self.search_str = search_str
        self.status_msg = Common.status_message_process("Hayoo '{0}'".format(search_str), priority=3)
        self.status_msg.start()
        BackendManager.active_backend().hayoo(self.search_str, page=0, pages=5, on_response=self.on_resp, on_error=self.on_err)

    def on_change(self, _ignored):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx >= 0:
            show_declaration_info(self.window.active_view(), self.decls[idx])

    def on_err(self, err, _details):
        self.status_msg.result_fail()
        Common.sublime_status_message("Hayoo '{0}': {1}".format(self.search_str, err))

    def on_resp(self, resp):
        self.status_msg.result_ok()
        self.decls = resp
        if not self.decls:
            Common.sublime_status_message("Hayoo '{0}': not found".format(self.search_str))
            return
        brief_decls = [[decl.module.name + ': ' + decl.brief(use_unicode=False),
                        decl.defined_module().location] for decl in self.decls]
        self.window.show_quick_panel(brief_decls, self.on_select)


class SublimeHaskellSearch(CommandWin.BackendWindowCommand):
    def __init__(self, win):
        super().__init__(win)
        self.search_str = ''
        self.decls = []
        self.status_msg = None

    def run(self, **_kwargs):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, search_str):
        self.search_str = search_str
        self.status_msg = Common.status_message_process("Search '{0}'".format(search_str), priority=3)
        self.status_msg.start()
        BackendManager.active_backend().symbol(lookup=self.search_str, search_type='infix',
                                               on_response=self.on_symbol, on_error=self.on_err)

    def on_change(self, _ignored):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx >= 0:
            show_declaration_info(self.window.active_view(), self.decls[idx])

    def on_err(self, err, _details):
        self.status_msg.result_fail()
        Common.sublime_status_message("Search '{0}': {1}".format(self.search_str, err))

    def on_symbol(self, resp):
        self.decls.extend(resp)
        BackendManager.active_backend().hayoo(self.search_str, page=0, pages=5, on_response=self.on_resp, on_error=self.on_err)

    def on_resp(self, resp):
        self.status_msg.result_ok()
        self.decls.extend(resp)
        if not self.decls:
            Common.sublime_status_message("Search '{0}' not found".format(self.search_str))
        else:
            hayoo_results = [[decl.module.name + ': ' + decl.brief(use_unicode=False),
                              decl.defined_module().location] for decl in self.decls]
            self.window.show_quick_panel(hayoo_results, self.on_select)


# General goto command
class SublimeHaskellGoTo(CommandWin.BackendWindowCommand):
    def __init__(self, win):
        super().__init__(win)
        self.view = win.active_view()
        self.decls = []
        self.declarations = []
        self.current_filename = self.view.file_name()
        self.line, self.column = self.view.rowcol(self.view.sel()[0].a)

    def run(self, **kwargs):
        project = kwargs.get('project') or False
        decls = []

        if project:
            project_name = Common.locate_cabal_project_from_view(self.view)[1]
            modules = BackendManager.active_backend().module(project_name, file=self.current_filename)
            current_project = Utils.head_of(modules).location.project
            if not current_project:
                Common.sublime_status_message('File {0} is not in project'.format(self.current_filename))
                return

            decls = self.sorted_decls_name(BackendManager.active_backend().symbol(project=current_project))
            self.declarations = [[decl.brief(True, use_unicode=False), decl.module.name] for decl in decls]
        else:
            decls = self.sorted_decls_pos(BackendManager.active_backend().symbol(file=self.current_filename, local_names=True))
            self.declarations = [[(decl.position.column * ' ') + decl.brief(True, use_unicode=False)] for decl in decls]
        self.decls = decls[:]

        if decls:
            self.window.show_quick_panel(self.declarations,
                                         self.on_done, 0,
                                         self.closest_idx(decls),
                                         self.on_highlighted if not project else None)

    def qualified_decls(self, decls):
        return [decl.make_qualified() for decl in decls]

    def sorted_decls_name(self, decls):
        return list(sorted(decls, key=lambda d: d.name))

    def sorted_decls_pos(self, decls):
        return list(sorted(decls, key=lambda d: (d.position.line, d.position.column)))

    def closest_idx(self, decls):
        fdecls = [decl for decl in enumerate(decls) if decl[1].defined_module().location.filename == self.current_filename]
        return min(fdecls, key=lambda d: abs(d[1].position.line - self.line))[0] if fdecls else -1

    def on_done(self, idx):
        if idx >= 0:
            self.open(self.decls[idx])

    def on_highlighted(self, idx):
        if idx >= 0:
            self.open(self.decls[idx], True)

    def open(self, decl, transient=False):
        self.window.open_file(decl.get_source_location(),
                              sublime.ENCODED_POSITION | sublime.TRANSIENT if transient else sublime.ENCODED_POSITION)


class SublimeHaskellGoToModule(CommandWin.BackendWindowCommand):
    def __init__(self, win):
        super().__init__(win)
        self.modules = []
        self.window = win

    def run(self, **_kwargs):
        self.modules = BackendManager.active_backend().list_modules(source=True)
        mod_strings = [[m.name if m.name != 'Main' else 'Main in {0}'.format(m.location.to_string()),
                        m.location.to_string()] for m in self.modules]
        self.window.show_quick_panel(mod_strings, self.on_done, 0, 0, self.on_highlighted)

    def on_done(self, idx):
        if idx >= 0:
            self.window.open_file(self.modules[idx].location.to_string())

    def on_highlighted(self, idx):
        if idx >= 0:
            self.window.open_file(self.modules[idx].location.to_string(), sublime.TRANSIENT)


class SublimeHaskellGoToHackagePackage(CommandWin.SublimeHaskellTextCommand):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, _edit, **_kwargs):
        pack = self.view.settings().get('package')
        if pack:
            webbrowser.open('http://hackage.haskell.org/package/{0}'.format(pack))

    def is_enabled(self):
        return self.view.settings().get('package') is not None

    def is_visible(self):
        return Common.view_is_haskell_symbol_info(self.view)


class SublimeHaskellGoToHackageModule(CommandWin.BackendTextCommand):
    def __init__(self, view):
        super().__init__(view)
        self.candidates = []

    def run(self, _edit, **_kwargs):
        if Common.view_is_haskell_symbol_info(self.view):
            pack = self.view.settings().get('package')
            mod = self.view.settings().get('module')
            if pack and mod:
                webbrowser.open('http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(pack, mod.replace('.', '-')))
        else:
            project_name = Common.locate_cabal_project_from_view(self.view)[1]
            qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])

            modules = []
            if qsymbol.is_module():  # module
                scope = self.view.file_name()
                if scope:
                    modules = [m for m in BackendManager.active_backend().scope_modules(project_name,
                                                                                        scope,
                                                                                        lookup=qsymbol.module,
                                                                                        search_type='exact')
                               if m.by_cabal()]
                else:
                    modules = [m for m in BackendManager.active_backend().list_modules(symdb=m.location.db)
                               if m.name == qsymbol.module and m.by_cabal()]
            else:  # symbol
                scope = self.view.file_name()
                if scope:
                    decls = BackendManager.active_backend().whois(qsymbol.qualified_name(), file=scope) or \
                            BackendManager.active_backend().lookup(qsymbol.full_name(), file=scope) or \
                            BackendManager.active_backend().symbol(lookup=qsymbol.full_name(), search_type='exact')
                    if not decls:
                        Common.sublime_status_message('Module for symbol {0} not found'.format(qsymbol.full_name()))
                        return
                    modules = [decl.defined_module() for decl in decls]

            if not modules:
                Common.sublime_status_message('Module {0} not found'.format(qsymbol.module))
            elif len(modules) == 1:
                pkg_id = modules[0].location.package.package_id()
                pkg_name = modules[0].name.replace('.', '-')
                webbrowser.open('http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(pkg_id, pkg_name))
            else:
                self.candidates = modules[:]
                mod_strings = [[m.name, m.location.package.package_id()] for m in self.candidates]
                self.view.window().show_quick_panel(mod_strings, self.on_done)

    def on_done(self, idx):
        if idx >= 0:
            pkg_id = self.candidates[idx].location.package.package_id()
            pkg_name = self.candidates[idx].name.replace('.', '-')
            webbrowser.open('http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(pkg_id, pkg_name))

    def is_enabled(self):
        return (self.view.settings().get('package') is not None) or \
               Common.view_is_haskell_source(self.view) or \
               Common.view_is_haskell_repl(self.view)

    def is_visible(self):
        return Common.view_is_haskell_symbol_info(self.view) or \
               Common.view_is_haskell_source(self.view) or \
               Common.view_is_haskell_repl(self.view)


## Not referenced in default commands or anywhere in the plugin:
## Deprecated for now:
##
# class SublimeHaskellGoToAnyDeclaration(CommandWin.SublimeHaskellWindowCommand):
#     def __init__(self, window):
#         super().__init__(window)
#         self.cache = None

#     def run(self):
#         with autocomplete.AutoCompletion().cache as cache_:
#             self.cache = cache_
#             self.window.show_quick_panel(cache_.source_locs, self.on_done)

#     def on_done(self, idx):
#         if idx >= 0:
#             self.window.open_file(self.cache.source_locs[idx][1], sublime.ENCODED_POSITION)


class SublimeHaskellReinspectAll(CommandWin.BackendWindowCommand):
    def run(self, **_kwargs):
        Utils.run_async('reinspect all', self.reinspect_all)

    def reinspect_all(self):
        Logging.log('reinspect all', Logging.LOG_TRACE)
        with BackendManager.inspector() as insp:
            insp.start_inspect()

class SublimeHaskellInferDocs(CommandWin.BackendTextCommand):
    """
    Infer types and scan docs for current module
    """
    def __init__(self, view):
        super().__init__(view)
        self.current_file_name = None
        self.status_msg = None

    def run(self, _edit, **kwargs):
        self.current_file_name = kwargs.get('filename') or self.view.file_name()
        self.status_msg = Common.status_message_process("Scanning docs for {0}".format(self.current_file_name), priority=3)
        self.status_msg.start()

        def run_infer():
            self.status_msg = Common.status_message_process("Inferring types for {0}".format(self.current_file_name),
                                                            priority=3)
            self.status_msg.start()

            def infer_on_resp(_resp):
                self.status_msg.result_ok()

            def infer_on_err(_err, _details):
                self.status_msg.result_fail()

            BackendManager.active_backend().infer(files=[self.current_file_name], on_response=infer_on_resp,
                                                  on_error=infer_on_err)

        def on_resp(_resp):
            self.status_msg.result_ok()
            run_infer()

        def on_err(_err, _details):
            self.status_msg.result_fail()
            run_infer()

        BackendManager.active_backend().docs(files=[self.current_file_name], on_response=on_resp, on_error=on_err)


class SublimeHaskellSymbolInfoCommand(CommandWin.BackendTextCommand):
    """
    Show information about selected symbol

    """
    def __init__(self, view):
        super().__init__(view)
        self.full_name = None
        self.current_file_name = None
        self.candidates = None
        self.candidate_selected = None
        self.whois_name = None

    def run(self, _edit, **kwargs):
        filename = kwargs.get('filename')
        module_name = kwargs.get('module_name')
        package_name = kwargs.get('package_name')
        symdb = kwargs.get('db')
        name = kwargs.get('name')
        qname = kwargs.get('qname')
        no_browse = kwargs.get('no_browse') or False

        if qname:
            self.full_name = qname
            self.current_file_name = self.view.file_name()
            # Try whois it, followed by file symbol and wider module searches
            self.candidates = self.collect_candidates(qname, name, filename, module_name, package_name, symdb)
        else:
            self.current_file_name = self.view.file_name()

            qsymbol = Common.get_qualified_symbol(qname) \
                      if qname \
                      else Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            module_word = qsymbol.module
            ident = qsymbol.name

            if ident is None:  # module
                if not no_browse:
                    self.view.window().run_command('sublime_haskell_browse_module', {'module_name': module_word,
                                                                                     'scope': self.current_file_name})
                return

            if not module_word and not ident:
                Common.sublime_status_message('No symbol selected')
                return

            self.whois_name = qsymbol.qualified_name()
            self.full_name = qsymbol.full_name()

            self.candidates = (BackendManager.active_backend().whois(self.whois_name, self.current_file_name) or [])[:1]

            if not self.candidates:
                self.candidates = BackendManager.active_backend().lookup(self.full_name, self.current_file_name)

            if not self.candidates:
                self.candidates = BackendManager.active_backend().symbol(lookup=self.full_name, search_type='exact')

        if not self.candidates:
            Common.sublime_status_message('Symbol {0} not found'.format(self.full_name))
        elif len(self.candidates) == 1:
            self.show_symbol_info(self.candidates[0])
        elif not no_browse:
            results = [[c.qualified_name(), c.defined_module().location.to_string()] for c in self.candidates]
            self.view.window().show_quick_panel(results, self.on_done)

    def on_done(self, idx):
        if idx >= 0:
            self.show_symbol_info(self.candidates[idx])

    def on_import_selected(self, idx):
        if idx == 0:  # Yes, select imported module
            results = ['{0}.{1}'.format(i[0], i[1]) for i in self.candidates]
            sublime.set_timeout(functools.partial(self.view.window().show_quick_panel, results, self.on_candidate_selected), 0)

    def on_candidate_selected(self, idx):
        if idx >= 0:
            (module_name, ident_name) = self.candidates[idx]
            info = BackendManager.active_backend().whois('{0}.{1}'.format(module_name, ident_name), self.view.file_name())

            if info:
                self.show_symbol_info(info[0])
            else:
                Common.sublime_status_message("Can't get info for {0}.{1}".format(module_name, ident_name),)

    def show_symbol_info(self, decl):
        show_declaration_info_panel(self.view, decl)

    def is_visible(self):
        return Common.view_is_haskell_source(self.view) or Common.view_is_haskell_repl(self.view)

    def collect_candidates(self, qualified_name, unqualified_name, filename, module_name, package_name, symdb):
        candidates = BackendManager.active_backend().whois(qualified_name, file=self.current_file_name)
        if not candidates:
            if filename:
                candidates = BackendManager.active_backend().symbol(lookup=unqualified_name, search_type='exact', file=filename)
            else:
                if module_name and package_name:
                    symbol_db = symbols.PackageDb.from_string(symdb) if symdb else None
                    candidates = BackendManager.active_backend().symbol(lookup=unqualified_name,
                                                                        search_type='exact',
                                                                        module=module_name,
                                                                        symdb=symbol_db,
                                                                        package=package_name)
                else:
                    candidates = []

        return candidates

TOGGLE_SYMBOL_INFO = False


class SublimeHaskellToggleSymbolInfoCommand(CommandWin.BackendWindowCommand):
    def run(self, **_kwargs):
        global TOGGLE_SYMBOL_INFO
        TOGGLE_SYMBOL_INFO = not TOGGLE_SYMBOL_INFO
        Common.sublime_status_message('continuous symbol info: {0}'.format('on' if TOGGLE_SYMBOL_INFO else 'off'))


class SublimeHaskellContinuousSymbolInfo(sublime_plugin.EventListener):
    def on_selection_modified(self, view):
        if TOGGLE_SYMBOL_INFO and Common.view_is_haskell_source(view) and view.file_name():
            view.run_command('sublime_haskell_symbol_info', {'no_browse': True})


class SublimeHaskellCleanImports(CommandWin.BackendTextCommand):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, edit, **kwargs):
        current_file_name = kwargs.get('filename', self.view.file_name())
        project_name = Common.locate_cabal_project_from_view(self.view)[1]
        backend = BackendManager.active_backend()

        imp_module = Utils.head_of(backend.module(project_name, file=current_file_name))
        if imp_module:
            imports = sorted(imp_module.imports, key=lambda i: i.position.line)

            supported, result = backend.clean_imports(current_file_name)
            print(result)
            if supported:
                if len(imports) == len(result):
                    Logging.log('replacing imports for {0}'.format(current_file_name), Logging.LOG_TRACE)
                    erased = 0
                    for imp, new_imp in zip(imports, result):
                        point = self.view.text_point(imp.position.line - 1 - erased, 0)
                        if new_imp.endswith('()'):
                            self.view.erase(edit, self.view.full_line(point))
                            erased = erased + 1
                        else:
                            self.view.replace(edit, self.view.line(point), new_imp)
                else:
                    Common.sublime_status_message('different number of imports: {0} and {1}'.format(len(imports), len(result)))
            else:
                if len(result) == 1:
                    Common.sublime_status_message(result[0])
                else:
                    sublime.message_dialog('\n'.join(result))
        else:
            Common.sublime_status_message('Clean Imports failed: module not scanned')


class SublimeHaskellBrowseModule(CommandWin.BackendTextCommand):
    """
    Browse module symbols
    """
    def __init__(self, view):
        super().__init__(view)
        self.candidates = None
        self.current_file_name = None

    def run(self, _edit, **kwargs):
        module_name = kwargs.get('module_name')
        filename = kwargs.get('filename')
        symdb = kwargs.get('db')
        scope = kwargs.get('scope')

        self.candidates = []
        self.current_file_name = self.view.window().active_view().file_name()

        the_module = None
        project_name = Common.locate_cabal_project_from_view(self.view.window().active_view())[1]

        if filename:
            the_module = Utils.head_of(BackendManager.active_backend().module(project_name, file=filename))
            if not the_module:
                Common.sublime_status_message('Module {0} not found'.format(filename))
                return
        elif module_name:
            cand_mods = self.candidate_modules(project_name, module_name, scope, symdb)
            if not cand_mods:
                Common.sublime_status_message('Module {0} not found'.format(module_name))
                return
            elif len(cand_mods) == 1:
                the_module = self.get_module_info(project_name, cand_mods[0], module_name)
                if the_module:
                    the_module = Utils.head_of(the_module)
            else:
                self.candidates.extend([(m, [m.name, m.location.to_string()]) for m in cand_mods])
        else:
            if self.current_file_name:
                cand_mods = BackendManager.active_backend().scope_modules(project_name, self.current_file_name)
            else:
                symbol_db = symbols.PackageDb.from_string(symdb) if symdb else None
                cand_mods = BackendManager.active_backend().list_modules(symdb=symbol_db)
            self.candidates.extend([(m, [m.name, m.location.to_string()]) for m in cand_mods])

        if the_module:
            self.candidates = sorted(list(the_module.declarations.values()), key=lambda d: d.brief())
            results = [[decl.brief(use_unicode=False),
                        decl.docs.splitlines()[0] if decl.docs else ''] for decl in self.candidates]
            self.view.window().show_quick_panel(results, self.on_symbol_selected)
        else:
            self.candidates.sort(key=lambda c: c[1][0])
            self.view.window().show_quick_panel([c[1] for c in self.candidates], self.on_done)

    def on_done(self, idx):
        if idx >= 0:
            the_module = self.candidates[idx][0]

            info = {}
            info['module_name'] = the_module.name
            if the_module.by_source():
                info['filename'] = the_module.location.filename
            if the_module.by_cabal() and the_module.location.package.name:
                info['package_name'] = the_module.location.package.name
                info['db'] = the_module.location.db.to_string()

            self.view.window().run_command('sublime_haskell_browse_module', info)

    def on_symbol_selected(self, idx):
        if idx >= 0:
            show_declaration_info(self.view.window().active_view(), self.candidates[idx])

    def candidate_modules(self, project_name, module_name, scope, symdb):
        retval = None
        if scope is not None:
            retval = BackendManager.active_backend().scope_modules(project_name, scope, lookup=module_name, search_type='exact')
        elif self.current_file_name is not None:
            retval = BackendManager.active_backend().scope_modules(project_name, self.current_file_name, lookup=module_name,
                                                                   search_type='exact')
        else:
            retval = BackendManager.active_backend().list_modules(module=module_name,
                                                                  symdb=symbols.PackageDb.from_string(symdb) if symdb else None)
        return retval

    def get_module_info(self, project_name, module, module_name):
        args = None
        if module.by_source():
            args = {'lookup': module_name, 'search_type': 'exact', 'file': module.location.filename}
        elif module.by_cabal():
            args = {'lookup': module_name, 'search_type': 'exact', 'symdb': module.location.db,
                    'package': module.location.package.name}
        else:
            args = {'lookup': module_name, 'search_type': 'exact'}

        return BackendManager.active_backend().module(project_name, **args) if args is not None else None

class SublimeHaskellGoToDeclaration(CommandWin.BackendTextCommand):
    def __init__(self, view):
        super().__init__(view)
        self.select_candidates = None

    def run(self, _edit, **_kwargs):
        qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])

        if Common.view_is_haskell_symbol_info(self.view):  # Go to within symbol info window
            loc = self.view.settings().get('location')
            if loc:
                self.view.window().open_file(loc, sublime.ENCODED_POSITION)
            else:
                Common.sublime_status_message('Source location of {0} not found'.format(qsymbol.name))
        else:
            backend = BackendManager.active_backend()
            whois_name = qsymbol.qualified_name()
            full_name = qsymbol.full_name()
            current_file_name = self.view.file_name()

            candidates = []
            module_candidates = []
            if not qsymbol.is_module():
                candidates = [decl for decl in backend.whois(whois_name, current_file_name) if decl.by_source()]
                if candidates:
                    if candidates[0].has_source_location():
                        self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                    else:
                        cands = candidates[:]
                        candidates = []
                        for cand in cands:
                            for i in cand.imported:
                                candidates = [sym for sym in backend.symbol(lookup=cand.name, search_type='exact', source=True)
                                              if sym.module.name == i.module]
                                if candidates and candidates[0].has_source_location():
                                    self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                    return
                else:
                    candidates = backend.symbol(lookup=qsymbol.name, search_type='exact', source=True)
            else:
                module_candidates = [m for m in backend.list_modules(source=True, module=full_name) if m.name == full_name]

            if not candidates and not module_candidates:
                Common.sublime_status_message('Declaration {0} not found'.format(qsymbol.name))
            else:
                candidates_len = len(candidates) if candidates is not None else 0
                module_candidates_len = len(module_candidates) if module_candidates is not None else 0

                if candidates_len + module_candidates_len == 1:
                    if candidates_len == 1:
                        self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                    elif module_candidates_len == 1:
                        self.view.window().open_file(module_candidates[0].location.filename)
                    return
                else:
                    # many candidates
                    self.select_candidates = [([c.brief(use_unicode=False), c.get_source_location()], True) for c in candidates]
                    self.select_candidates += [([m.name, m.location.filename], False) for m in module_candidates]

                    just_names = [c[0] for c in self.select_candidates]
                    self.view.window().show_quick_panel(just_names, self.on_done, 0, 0, self.on_highlighted)

    def on_done(self, idx):
        if idx >= 0:
            selected = self.select_candidates[idx]
            if selected[1]:
                self.view.window().open_file(selected[0][1], sublime.ENCODED_POSITION)
            else:
                self.view.window().open_file(selected[0][1])

    def on_highlighted(self, idx):
        if idx >= 0:
            selected = self.select_candidates[idx]
            if selected[1]:
                self.view.window().open_file(selected[0][1], sublime.ENCODED_POSITION | sublime.TRANSIENT)
            else:
                self.view.window().open_file(selected[0][1], sublime.TRANSIENT)

    def is_enabled(self):
        return Common.view_is_haskell_source(self.view) or Common.view_is_haskell_repl(self.view) or \
               (Common.view_is_haskell_symbol_info(self.view) and self.view.settings().get('location'))


class SublimeHaskellEvalReplaceCommand(CommandWin.SublimeHaskellTextCommand):
    def run(self, edit, **kwargs):
        results = kwargs.get('results') or []

        for i, res in enumerate(results):
            if res is not None:
                self.view.replace(edit, self.view.sel()[i], str(res))
        for j in range(len(results), len(self.view.sel())):
            self.view.erase(edit, self.view.sel()[j])

    def is_enabled(self):
        return True


def ghc_eval_x(resps):
    # Drop 'fail' to be 'None' and unwrap strings
    # No idea how to call this function
    def process(i):
        if isinstance(i, dict):
            return None
        try:
            resp_str = json.loads(i)
            if isinstance(resp_str, str):
                return resp_str
        except ValueError:
            pass

        return i
    return list(map(process, resps))


def ghc_eval_merge_results(left, right):
    # Prefer result in 'l', but if there's 'fail' - use result from 'r'
    return [x or y for x, y in zip(left, right)]


class SublimeHaskellEvalSelectionCommand(CommandWin.BackendTextCommand):
    def __init__(self, view):
        super().__init__(view)
        self.args = []
        self.results = None

    def run(self, _edit, **_kwargs):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.results = ghc_eval_x(BackendManager.active_backend().ghc_eval(self.args))

        self.view.run_command('sublime_haskell_eval_replace', {'results': self.results})

    def is_enabled(self):
        return True

    def is_visible(self):
        return True


class SublimeHaskellApplyToSelectionCommand(CommandWin.BackendTextCommand):
    def __init__(self, view):
        super().__init__(view)
        self.args = []
        self.edit = None
        self.results = None
        self.string_results = None

    def run(self, edit, **_kwargs):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.edit = edit
        self.view.window().show_input_panel('Function', '', self.on_done, None, self.on_cancel)

    def is_enabled(self):
        return True

    def is_visible(self):
        return True

    def on_done(self, fname):
        self.results = ghc_eval_x(BackendManager.active_backend().ghc_eval(["({0}) ({1})".format(fname, a) for a in self.args]))
        ghc_eval_expr = ["({0}) ({1})".format(fname, json.dumps(a)) for a in self.args]
        self.string_results = ghc_eval_x(BackendManager.active_backend().ghc_eval(ghc_eval_expr))

        self.view.run_command('sublime_haskell_eval_replace',
                              {'results': ghc_eval_merge_results(self.results, self.string_results)})

    def on_cancel(self):
        pass


class SublimeHaskellApplyToSelectionListCommand(CommandWin.BackendTextCommand):
    def __init__(self, view):
        super().__init__(view)
        self.args = []
        self.edit = None
        self.results = None
        self.string_results = None
        self.res = None

    def run(self, edit, **_kwargs):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.edit = edit
        self.view.window().show_input_panel('Function', '', self.on_done, None, self.on_cancel)

    def is_enabled(self):
        return True

    def is_visible(self):
        return True

    def on_done(self, fname):
        comma_delim_args = ", ".join(self.args)
        json_args = ", ".join([json.dumps(a) for a in self.args])

        self.results = ghc_eval_x(BackendManager.active_backend().ghc_eval(['({0}) [{1}]'.format(fname, comma_delim_args)]))
        self.string_results = ghc_eval_x(BackendManager.active_backend().ghc_eval(['({0}) [{1}]'.format(fname, json_args)]))
        self.res = ghc_eval_merge_results(self.results, self.string_results)

        if self.res[0] is None:
            return
        else:
            result_list = json.loads(self.res[0])
            self.view.run_command('sublime_haskell_eval_replace', {'results': result_list})

    def on_cancel(self):
        pass


class AutoFixState(object):
    def __init__(self, view=None, corrections=None, selected=0, undo_history=None, redo_history=None):
        self.view = view
        self.corrections = corrections or []
        self.selected = selected
        self.undo_history = undo_history[:] if undo_history is not None else []
        self.redo_history = redo_history[:] if redo_history is not None else []

    def is_active(self):
        return self.view and self.corrections

    def set(self, view, corrections, selected=0, undo_history=None, redo_history=None):
        if self.is_active():
            self.clear()
        self.view = view
        self.view.settings().set('autofix', True)
        self.view.set_read_only(True)
        self.corrections = corrections or []
        self.selected = selected
        self.undo_history = undo_history[:] if undo_history is not None else []
        self.redo_history = redo_history[:] if redo_history is not None else []

    def clear(self):
        if self.view:
            self.view.set_read_only(False)
            self.view.settings().erase('autofix')
            self.unmark()
            self.view = None
            self.corrections = []
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
        Common.output_panel(self.view.window(),
                            'Press {0}\n\n{1}'.format(self.keys(), self.message(cur)),
                            'sublime_haskell_auto_fix',
                            syntax='HaskellAutoFix')

    def keys(self):
        return u'\u2191: Prev | \u2193: Next | \u21b5: Accept | ESC: Cancel'

    def message(self, cur):
        if cur.corrector.contents:
            return u'\u2014 {0}\n  Why not:\n\n{1}'.format(cur.message, cur.corrector.contents)
        return u'\u2014 {0}'.format(cur.message)

    def unmark(self):
        self.view.erase_regions('autofix')
        self.view.erase_regions('autofix_current')
        window = self.view.window()
        Common.hide_panel(window, panel_name='sublime_haskell_auto_fix')

    def count(self):
        return len(self.corrections)

    def set_selected(self, i):
        if i < 0 or i >= len(self.corrections):
            Logging.log('AutoFixState.set_selected({0}): out of bound'.format(i), Logging.LOG_ERROR)
            return
        self.selected = i
        self.mark()

    def fix_current(self):
        self.undo_history.append((self.corrections[:], self.selected))
        self.redo_history.clear()
        (cur, corrs) = self.get_corrections()
        self.corrections = BackendManager.active_backend().autofix_fix(HsDevResultParse.encode_corrections([cur]),
                                                                       rest=HsDevResultParse.encode_corrections(corrs),
                                                                       pure=True)
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

AUTOFIX_STATE = AutoFixState()


class SublimeHaskellAutoFix(CommandWin.BackendWindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.status_msg = None

    def run(self, **_kwargs):
        if self.window.active_view().file_name():
            def on_resp(msgs):
                self.status_msg.result_ok()
                if msgs:
                    sublime.set_timeout(functools.partial(self.on_got_messages, msgs), 0)

            def on_err(err, _details):
                self.status_msg.result_fail()
                Common.sublime_status_message('Check & Lint: {0}'.format(err))

            self.status_msg = Common.status_message_process('Autofix: ' + self.window.active_view().file_name(), priority=3)
            self.status_msg.start()
            BackendManager.active_backend().check_lint(files=[self.window.active_view().file_name()],
                                                       ghc=Settings.PLUGIN.ghc_opts,
                                                       on_response=on_resp,
                                                       on_error=on_err)

    def on_got_messages(self, msgs):
        fixes = BackendManager.active_backend().autofix_show(msgs, wait_complete=True)
        corrections = [corr for corr in fixes if os.path.samefile(corr.file, self.window.active_view().file_name())]
        if corrections:
            AUTOFIX_STATE.set(self.window.active_view(), corrections)
            AUTOFIX_STATE.mark()


class SublimeHaskellAutoFixTextBase(CommandWin.SublimeHaskellTextCommand):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def is_enabled(self):
        return AUTOFIX_STATE.is_active() and AUTOFIX_STATE.view == self.view


class SublimeHaskellAutoFixWindowBase(CommandWin.SublimeHaskellWindowCommand):
    ## Uncomment if instance variables are needed.
    # def __init__(self, window):
    #     super().__init__(window)

    def is_enabled(self):
        return AUTOFIX_STATE.is_active() and AUTOFIX_STATE.view == self.window.active_view()


class SublimeHaskellAutoFixPrevious(SublimeHaskellAutoFixWindowBase):
    ## Uncomment if instance variables are needed.
    # def __init__(self, window):
    #     super().__init__(window)

    def run(self, **_kwargs):
        if AUTOFIX_STATE.selected > 0:
            AUTOFIX_STATE.set_selected(AUTOFIX_STATE.selected - 1)


class SublimeHaskellAutoFixNext(SublimeHaskellAutoFixWindowBase):
    ## Uncomment if instance variables are needed.
    # def __init__(self, window):
    #     super().__init__(window)

    def run(self, **_kwargs):
        if AUTOFIX_STATE.selected + 1 < AUTOFIX_STATE.count():
            AUTOFIX_STATE.set_selected(AUTOFIX_STATE.selected + 1)


class SublimeHaskellAutoFixAll(SublimeHaskellAutoFixWindowBase):
    ## Uncomment if instance variables are needed.
    # def __init__(self, window):
    #     super().__init__(window)

    def run(self, **_kwargs):
        AUTOFIX_STATE.unmark()
        self.window.active_view().run_command('sublime_haskell_auto_fix_fix_it', {'all': True})
        AUTOFIX_STATE.clear()


class SublimeHaskellAutoFixFix(SublimeHaskellAutoFixWindowBase):
    ## Uncomment if instance variables are needed.
    # def __init__(self, window):
    #     super().__init__(window)

    def run(self, **_kwargs):
        AUTOFIX_STATE.unmark()
        self.window.active_view().run_command('sublime_haskell_auto_fix_fix_it')
        AUTOFIX_STATE.fix_current()


class SublimeHaskellAutoFixFixIt(SublimeHaskellAutoFixTextBase):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, edit, **kwargs):
        everything = kwargs.get('all') or False
        corrections = AUTOFIX_STATE.corrections[:] if everything else [AUTOFIX_STATE.current_correction()]
        if corrections:
            corrs = sorted([(correction.to_region(self.view), correction.corrector.contents) for correction in corrections],
                           key=lambda c: c[0])

            self.view.set_read_only(False)
            rgns = [c[0] for c in corrs]
            # self.view.add_regions('autofix_fix', [c[0] for c in corrs], 'warning', 'dot', sublime.HIDDEN)

            for _, cts in corrs:
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
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, **_kwargs):
        AUTOFIX_STATE.unmark()
        self.window.active_view().set_read_only(False)
        self.window.active_view().run_command('undo')
        self.window.active_view().set_read_only(True)
        AUTOFIX_STATE.undo()

    def is_enabled(self):
        return super().is_enabled() and AUTOFIX_STATE.has_undo()


class SublimeHaskellAutoFixRedo(SublimeHaskellAutoFixWindowBase):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, **_kwargs):
        AUTOFIX_STATE.unmark()
        self.window.active_view().set_read_only(False)
        self.window.active_view().run_command('redo')
        self.window.active_view().set_read_only(True)
        AUTOFIX_STATE.redo()

    def is_enabled(self):
        return super().is_enabled() and AUTOFIX_STATE.has_redo()


class SublimeHaskellAutoFixStop(SublimeHaskellAutoFixWindowBase):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, **_kwargs):
        AUTOFIX_STATE.clear()


class SublimeHaskellReplaceRegions(sublime_plugin.TextCommand):
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

    def run(self, edit, **kwargs):
        replaces = kwargs.get('replaces')
        self.view.add_regions('sublime_haskell_replace_regions',
                              [sublime.Region(start, end) for (start, end), text in replaces],
                              'warning',
                              'dot',
                              sublime.HIDDEN)
        repl_rgns = self.view.get_regions('sublime_haskell_replace_regions')
        for i, (_rgn, text) in enumerate(replaces):
            self.view.replace(edit, repl_rgns[i], text)
        self.view.erase_regions('sublime_haskell_replace_regions')


class SublimeHaskellStackConfigSwitch(CommandWin.SublimeHaskellWindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.view = window.active_view()

    def run(self, **_kwargs):
        options = Settings.get_project_setting(self.view, 'stack_config_file_list', [])
        self.view.window().show_quick_panel(options, self.on_done)

    def on_done(self, idx):
        options = Settings.get_project_setting(self.view, 'stack_config_file_list')
        selected = options[idx]

        Settings.set_project_setting(self.view, 'active_stack_config', selected)
