import json
import os.path
import shlex
import threading
import webbrowser

import sublime
import sublime_plugin

if int(sublime.version()) < 3000:
    import sublime_haskell_common as Common
    import internals.logging as Logging
    import internals.proc_helper as ProcHelper
    import internals.settings as Settings
    import internals.output_collector as OutputCollector
    import internals.utils as Utils
    import autocomplete
    import symbols
    import hsdev
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.logging as Logging
    import SublimeHaskell.internals.proc_helper as ProcHelper
    import SublimeHaskell.internals.settings as Settings
    import SublimeHaskell.internals.output_collector as OutputCollector
    import SublimeHaskell.internals.utils as Utils
    import SublimeHaskell.autocomplete as autocomplete
    import SublimeHaskell.symbols as symbols
    import SublimeHaskell.hsdev as hsdev

# Extract the filename, line, column from symbol info
symbol_file_regex = r'^Defined at: (.*):(\d+):(\d+)$'


def is_scanned_source(view=None):
    window, view, file_shown_in_view = Common.get_haskell_command_window_view_file_project(view)
    if file_shown_in_view is None:
        return False
    m = Utils.head_of(hsdev.client.module(file=file_shown_in_view))
    return m is not None


def is_in_project(view=None):
    window, view, file_shown_in_view = Common.get_haskell_command_window_view_file_project(view)
    if file_shown_in_view is None:
        return False
    m = Utils.head_of(hsdev.client.module(file = file_shown_in_view))
    if m is None:
        return False
    return m.location.project is not None


def show_declaration_info_panel(view, decl):
    info = decl.detailed()
    v = Common.output_panel(view.window(), info, 'sublime_haskell_symbol_info_panel', syntax='HaskellSymbolInfo')
    v.settings().set('result_file_regex', symbol_file_regex)
    v.settings().erase('location')
    v.settings().erase('package')
    v.settings().erase('module')
    if decl.has_source_location():
        v.settings().set('location', decl.get_source_location())
    if decl.by_cabal():
        v.settings().set('package', decl.defined_module().location.package.package_id())
        v.settings().set('module', decl.defined_module().name)


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


class SublimeHaskellContext(sublime_plugin.EventListener):
    def on_query_context(self, view, key, operator, operand, match_all):
        if key == 'haskell_autofix':
            return view.settings().get('autofix')
        if key == 'auto_completion_popup':
            return Settings.get_setting('auto_completion_popup')
        elif key == 'haskell_source':
            return Common.is_haskell_source(view)
        elif key == 'haskell_source_or_repl':
            return Common.is_haskell_source(view) or Common.is_haskell_repl(view)
        elif key == 'haskell_repl':
            return Common.is_haskell_repl(view)
        elif key == 'haskell_symbol_info':
            return Common.is_haskell_symbol_info(view)
        elif key == 'cabal_source':
            return Common.is_cabal_source(view)
        elif key == 'scanned_source':
            return is_scanned_source(view)
        elif key == 'in_project':
            return is_in_project(view)
        elif key == "is_module_completion" or key == "is_import_completion":
            chars = {
                "is_module_completion": '.',
                "is_import_completion": '('}

            region = view.sel()[0]
            if region.a != region.b:
                return False
            word_region = view.word(region)
            preline = Common.get_line_contents_before_region(view, word_region)
            preline += chars[key]
            return autocomplete.can_complete_qualified_symbol(Common.get_qualified_symbol(preline))
        else:
            return False


class SublimeHaskellComplete(Common.SublimeHaskellTextCommand):
    """ Shows autocompletion popup """
    def run(self, edit, characters, wide=False):
        self.wide = wide
        if characters:
            for region in self.view.sel():
                self.view.insert(edit, region.end(), characters)

        # if can_complete_qualified_symbol(Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])):
        self.view.run_command("hide_auto_complete")
        sublime.set_timeout(self.do_complete, 1)

    def do_complete(self):
        if self.wide:
            autocomplete.autocompletion.mark_wide_completion(self.view)
        self.view.run_command("auto_complete")


class SublimeHaskellBrowseDeclarations(hsdev.HsDevTextCommand):
    """
    Show all available declarations in scope
    """
    def run(self, edit):
        self.decls = []
        self.current_file_name = self.view.file_name()
        self.status_msg = Common.status_message_process('Browse declarations', priority=3)
        self.status_msg.start()
        hsdev.client.scope(self.current_file_name, wait=False, on_response=self.on_resp, on_error=self.on_err)

    def on_resp(self, rs):
        self.decls = rs
        self.status_msg.stop()
        self.view.window().show_quick_panel([[decl.brief(use_unicode=False), decl.docs.splitlines()[0] if decl.docs else ''] for decl in self.decls], self.on_done)

    def on_err(self, e, ds):
        self.status_msg.fail()
        self.status_msg.stop()
        Common.show_status_message('Browse declarations: {0}'.format(e))

    def on_done(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.view, self.decls[idx])


class SublimeHaskellFindDeclarations(hsdev.HsDevWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.decls = hsdev.client.symbol(input=input, search_type='regex')
        if not self.decls:
            Common.show_status_message("Nothing found for: {0}".format(input))
            return

        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(use_unicode=False), str(decl.defined_module().location)] for decl in self.decls], self.on_select)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.window.active_view(), self.decls[idx])


class SublimeHaskellHayoo(hsdev.HsDevWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.input = input
        self.status_msg = Common.status_message_process("Hayoo '{0}'".format(self.input), priority=3)
        self.status_msg.start()
        hsdev.client.hayoo(self.input, page=0, pages=5, on_response=self.on_resp, on_error=self.on_err, wait=False)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.window.active_view(), self.decls[idx])

    def on_err(self, e, ds):
        self.status_msg.fail()
        self.status_msg.stop()
        Common.show_status_message("Hayoo '{0}': {1}".format(self.input, e))

    def on_resp(self, rs):
        self.status_msg.stop()
        self.decls = rs
        if not self.decls:
            Common.show_status_message("Hayoo '{0}': not found".format(self.input))
            return
        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(use_unicode=False), str(decl.defined_module().location)] for decl in self.decls], self.on_select)


class SublimeHaskellSearch(hsdev.HsDevWindowCommand):
    def run(self):
        self.window.show_input_panel("Search string", "", self.on_done, self.on_change, self.on_cancel)

    def on_done(self, input):
        self.input = input
        self.decls = []
        self.status_msg = Common.status_message_process("Search '{0}'".format(self.input), priority=3)
        self.status_msg.start()
        hsdev.client.symbol(input=self.input, search_type='infix', wait=False, on_response=self.on_symbol, on_error=self.on_err)

    def on_change(self, input):
        pass

    def on_cancel(self):
        pass

    def on_select(self, idx):
        if idx == -1:
            return
        show_declaration_info(self.window.active_view(), self.decls[idx])

    def on_err(self, e, ds):
        self.status_msg.fail()
        self.status_msg.stop()
        Common.show_status_message("Search '{0}': {1}".format(self.input, e))

    def on_symbol(self, rs):
        self.decls.extend(rs)
        hsdev.client.hayoo(self.input, page=0, pages=5, wait=False, on_response=self.on_resp, on_error=self.on_err)

    def on_resp(self, rs):
        self.status_msg.stop()
        self.decls.extend(rs)
        if not self.decls:
            Common.show_status_message("Search '{0}' not found".format(self.input))
            return
        self.window.show_quick_panel([[decl.module.name + ': ' + decl.brief(use_unicode=False), str(decl.defined_module().location)] for decl in self.decls], self.on_select)


# General goto command
class SublimeHaskellGoTo(hsdev.HsDevWindowCommand):
    def run(self, project=False):
        self.decls = []
        self.declarations = []
        decls = []

        self.view = self.window.active_view()
        self.current_filename = self.view.file_name()
        (self.line, self.column) = self.view.rowcol(self.view.sel()[0].a)

        if project:
            current_project = Utils.head_of(hsdev.client.module(file=self.current_filename)).location.project
            if not current_project:
                Common.show_status_message('File {0} is not in project'.format(self.current_filename), False)
                return

            decls = self.sorted_decls_name(hsdev.client.symbol(project=current_project))
            self.declarations = [[decl.brief(True, use_unicode=False), decl.module.name] for decl in decls]
        else:
            decls = self.sorted_decls_pos(hsdev.client.symbol(file=self.current_filename, locals=True))
            self.declarations = [[(decl.position.column * ' ') + decl.brief(True, use_unicode=False)] for decl in decls]
        self.decls = decls[:]

        if not decls:
            return

        self.window.show_quick_panel(self.declarations, self.on_done, 0, self.closest_idx(decls), self.on_highlighted if not project else None)

    def qualified_decls(self, decls):
        for decl in decls:
            decl.make_qualified()
        return decls

    def sorted_decls_name(self, decls):
        return list(sorted(decls, key=lambda d: d.name))

    def sorted_decls_pos(self, decls):
        return list(sorted(decls, key=lambda d: (d.position.line, d.position.column)))

    def closest_idx(self, decls):
        fdecls = list(filter(
            lambda d: d[1].defined_module().location.filename == self.current_filename,
            enumerate(decls)))
        if not fdecls:
            return -1
        return min(fdecls, key=lambda d: abs(d[1].position.line - self.line))[0]

    def on_done(self, idx):
        if idx == -1:
            return
        self.open(self.decls[idx])

    def on_highlighted(self, idx):
        if idx == -1:
            return
        self.open(self.decls[idx], True)

    def open(self, decl, transient=False):
        self.window.open_file(decl.get_source_location(), sublime.ENCODED_POSITION | sublime.TRANSIENT if transient else sublime.ENCODED_POSITION)


class SublimeHaskellGoToModule(hsdev.HsDevWindowCommand):
    def run(self):
        self.modules = hsdev.client.list_modules(source=True)
        self.window.show_quick_panel([[m.name if m.name != 'Main' else 'Main in {0}'.format(m.location.to_string()), m.location.to_string()] for m in self.modules], self.on_done, 0, 0, self.on_highlighted)

    def on_done(self, idx):
        if idx == -1:
            return
        self.window.open_file(self.modules[idx].location.to_string())

    def on_highlighted(self, idx):
        if idx == -1:
            return

        self.window.open_file(self.modules[idx].location.to_string(), sublime.TRANSIENT)


class SublimeHaskellGoToHackagePackage(Common.SublimeHaskellTextCommand):
    def run(self, edit):
        pack = self.view.settings().get('package')
        if pack:
            webbrowser.open('http://hackage.haskell.org/package/{0}'.format(pack))

    def is_enabled(self):
        return self.view.settings().get('package') is not None

    def is_visible(self):
        return Common.is_haskell_symbol_info(self.view)


class SublimeHaskellGoToHackageModule(hsdev.HsDevTextCommand):
    def run(self, edit):
        if Common.is_haskell_symbol_info(self.view):
            pack = self.view.settings().get('package')
            mod = self.view.settings().get('module')
            if pack and mod:
                webbrowser.open('http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(pack, mod.replace('.', '-')))
        else:
            qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])

            ms = []
            if qsymbol.is_module():  # module
                scope = self.view.file_name()
                if scope:
                    ms = [m for m in hsdev.client.scope_modules(
                        scope, input=qsymbol.module, search_type='exact') if m.by_cabal()]
                else:
                    ms = [m for m in hsdev.client.list_modules(db=m.location.db) if m.name == qsymbol.module and m.by_cabal()]
            else:  # symbol
                scope = self.view.file_name()
                if scope:
                    decls = hsdev.client.whois(
                        qsymbol.qualified_name(),
                        file=scope)
                    if not decls:
                        decls = hsdev.client.lookup(
                            qsymbol.full_name(),
                            file=scope)
                    if not decls:
                        decls = hsdev.client.symbol(
                            input=qsymbol.full_name(),
                            search_type='exact')
                    if not decls:
                        Common.show_status_message('Module for symbol {0} not found'.format(qsymbol.full_name()))
                        return
                    ms = [decl.defined_module() for decl in decls]

            if len(ms) == 0:
                Common.show_status_message('Module {0} not found'.format(module_name))
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
        return (self.view.settings().get('package') is not None) or Common.is_haskell_source(self.view) or Common.is_haskell_repl(self.view)

    def is_visible(self):
        return Common.is_haskell_symbol_info(self.view) or Common.is_haskell_source(self.view) or Common.is_haskell_repl(self.view)


class SublimeHaskellGoToAnyDeclaration(Common.SublimeHaskellWindowCommand):
    def run(self):
        with autocomplete.autocompletion.cache as cache_:
            self.cache_ = cache_
            self.window.show_quick_panel(cache_.source_locs, self.on_done)

    def on_done(self, idx):
        if idx == -1:
            return
        self.window.open_file(self.cache_.source_locs[idx][1], sublime.ENCODED_POSITION)


class SublimeHaskellReinspectAll(hsdev.HsDevWindowCommand):
    def run(self):
        if hsdev.agent_connected():
            Logging.log('reinspect all', Logging.LOG_TRACE)
            hsdev.agent.start_inspect()
        else:
            Common.show_status_message("inspector not connected", is_ok=False)


class SublimeHaskellScanContents(hsdev.HsDevTextCommand):
    """
    Scan module contents
    """
    def run(self, edit, filename=None):
        self.current_file_name = filename or self.view.file_name()
        self.status_msg = Common.status_message_process("Scanning {0}".format(self.current_file_name), priority=3)
        self.status_msg.start()

        def on_resp(r):
            self.status_msg.stop()
            autocomplete.update_completions_async([self.current_file_name])

        def on_err(r, ds):
            self.status_msg.fail()
            self.status_msg.stop()

        hsdev.client.scan(contents={self.current_file_name: self.view.substr(sublime.Region(0, self.view.size()))}, on_response=on_resp, on_error=on_err)


class SublimeHaskellInferDocs(hsdev.HsDevTextCommand):
    """
    Infer types and scan docs for current module
    """
    def run(self, edit, filename=None):
        self.current_file_name = filename or self.view.file_name()
        self.status_msg = Common.status_message_process("Scanning docs for {0}".format(self.current_file_name), priority=3)
        self.status_msg.start()

        def run_infer():
            self.status_msg = Common.status_message_process("Inferring types for {0}".format(self.current_file_name), priority=3)
            self.status_msg.start()

            def on_resp_(r):
                self.status_msg.stop()

            def on_err_(e):
                self.status_msg.fail()
                self.status_msg.stop()

            hsdev.client.infer(files=[self.current_file_name], on_response=on_resp_, on_error=on_err_)

        def on_resp(r):
            self.status_msg.stop()
            run_infer()

        def on_err(e):
            self.status_msg.fail()
            self.status_msg.stop()
            run_infer()

        hsdev.client.docs(files=[self.current_file_name], on_response=on_resp, on_error=on_err)


class SublimeHaskellSymbolInfoCommand(hsdev.HsDevTextCommand):
    """
    Show information about selected symbol

    """
    def run(self, edit, filename=None, module_name=None, package_name=None, db=None, name=None, qname=None, no_browse=False):
        if qname:
            self.full_name = qname
            self.current_file_name = self.view.file_name()
            # Try whois it
            self.candidates = hsdev.client.whois(qname, file=self.current_file_name)
            if not self.candidates:
                if filename:
                    self.candidates = hsdev.client.symbol(name, search_type='exact', file=filename)
                elif module_name and package_name and db:
                    self.candidates = hsdev.client.symbol(
                        name,
                        search_type='exact',
                        module=module_name,
                        db=symbols.PackageDb.from_string(db) if db else None,
                        package=package_name)
        else:
            self.current_file_name = self.view.file_name()

            qsymbol = Common.get_qualified_symbol(qname) if qname else Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            module_word = qsymbol.module
            ident = qsymbol.name

            if ident is None:  # module
                if not no_browse:
                    self.view.window().run_command('sublime_haskell_browse_module', {
                        'module_name': module_word,
                        'scope': self.current_file_name})
                return

            if not module_word and not ident:
                Common.show_status_message('No symbol selected', False)
                return

            self.whois_name = qsymbol.qualified_name()
            self.full_name = qsymbol.full_name()

            self.candidates = (hsdev.client.whois(self.whois_name, self.current_file_name) or [])[:1]

            if not self.candidates:
                self.candidates = hsdev.client.lookup(self.full_name, self.current_file_name)

            if not self.candidates:
                self.candidates = hsdev.client.symbol(input=self.full_name, search_type='exact')

        if not self.candidates:
            Common.show_status_message('Symbol {0} not found'.format(self.full_name))
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
        if idx == 0:  # Yes, select imported module
            sublime.set_timeout(
                lambda: self.view.window().show_quick_panel(['{0}.{1}'.format(i[0], i[1]) for i in self.candidates], self.on_candidate_selected), 0)

    def on_candidate_selected(self, idx):
        if idx == -1:
            return

        (module_name, ident_name) = self.candidates[idx]
        info = hsdev.client.whois('{0}.{1}'.format(module_name, ident_name), self.view.file_name())

        if info:
            self.show_symbol_info(info[0])
        else:
            Common.show_status_message("Can't get info for {0}.{1}".format(module_name, ident_name), False)

    def show_symbol_info(self, decl):
        show_declaration_info_panel(self.view, decl)

    def is_visible(self):
        return Common.is_haskell_source(self.view) or Common.is_haskell_repl(self.view)

toggle_symbol_info = False


class SublimeHaskellToggleSymbolInfoCommand(hsdev.HsDevWindowCommand):
    def run(self):
        global toggle_symbol_info
        toggle_symbol_info = not toggle_symbol_info
        Common.show_status_message('continuous symbol info: {0}'.format('on' if toggle_symbol_info else 'off'))


class SublimeHaskellContinuousSymbolInfo(sublime_plugin.EventListener):
    def on_selection_modified(self, view):
        if toggle_symbol_info:
            if Common.is_haskell_source(view) and view.file_name():
                view.run_command('sublime_haskell_symbol_info', {'no_browse': True})


class SublimeHaskellInsertImportForSymbol(hsdev.HsDevTextCommand):
    """
    Insert import for symbol
    """
    def run(self, edit, filename=None, decl=None, module_name=None):
        self.full_name = decl
        self.current_file_name = filename
        self.edit = edit

        if module_name is not None:
            self.add_import(module_name)
            return

        if not self.current_file_name:
            self.current_file_name = self.view.file_name()

        if not self.full_name:
            qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            self.full_name = qsymbol.qualified_name()

        if hsdev.client.whois(self.full_name, self.current_file_name):
            Common.show_status_message('Symbol {0} already in scope'.format(self.full_name))
            return

        self.candidates = hsdev.client.lookup(self.full_name, self.current_file_name)

        if not self.candidates:
            Common.show_status_message('Symbol {0} not found'.format(self.full_name))
            return

        if len(self.candidates) == 1:
            self.add_import(self.candidates[0].module.name)
            return

        self.view.window().show_quick_panel([[c.module.name] for c in self.candidates], self.on_done)

    def add_import(self, module_name):
        self.module_name = module_name
        contents = self.view.substr(sublime.Region(0, self.view.size()))
        contents_part = contents[0: list(re.finditer('^import.*$', contents, re.MULTILINE))[-1].end()]
        ProcHelper.ProcHelper.invoke_tool(['hsinspect'], 'hsinspect', contents_part, self.on_inspected, check_enabled=False)

    def on_inspected(self, result):
        cur_module = hsdev.parse_module(json.loads(result)['module']) if self.view.is_dirty() else Utils.head_of(hsdev.client.module(file=self.current_file_name))
        imports = sorted(cur_module.imports, key=lambda i: i.position.line)
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

        Common.show_status_message('Import {0} added'.format(self.module_name), True)

    def on_done(self, idx):
        if idx == -1:
            return
        self.view.run_command('sublime_haskell_insert_import_for_symbol', {
            'filename': self.current_file_name,
            'module_name': self.candidates[idx].module.name})

    def is_visible(self):
        return Common.is_haskell_source(self.view)


class SublimeHaskellClearImports(hsdev.HsDevTextCommand):
    def run(self, edit, filename=None):
        self.current_file_name = filename
        self.edit = edit

        if not self.current_file_name:
            self.current_file_name = self.view.file_name()

        cur_module = Utils.head_of(hsdev.client.module(file=self.current_file_name))
        if not cur_module:
            Logging.log("module not scanned")
            return

        imports = sorted(cur_module.imports, key=lambda i: i.position.line)

        cmd = ['hsclearimports', self.current_file_name, '--max-import-list', '32']
        exit_code, cleared, err = ProcHelper.ProcHelper.run_process(cmd)
        if exit_code != 0:
            Logging.log('hsclearimports error: {0}'.format(err), Logging.LOG_ERROR)
            return

        new_imports = cleared.splitlines()

        if len(imports) != len(new_imports):
            Logging.log('different number of imports: {0} and {1}'.format(len(imports), len(new_imports)), Logging.LOG_ERROR)
            return

        Logging.log('replacing imports for {0}'.format(self.current_file_name), Logging.LOG_TRACE)
        erased = 0
        for i, ni in zip(imports, new_imports):
            pt = self.view.text_point(i.position.line - 1 - erased, 0)
            if ni.endswith('()'):
                self.view.erase(edit, self.view.full_line(pt))
                erased = erased + 1
            else:
                self.view.replace(edit, self.view.line(pt), ni)


class SublimeHaskellBrowseModule(hsdev.HsDevWindowCommand):
    """
    Browse module symbols
    """
    def run(self, module_name=None, package_name=None, filename=None, db=None, scope=None):
        self.candidates = []
        self.current_file_name = self.window.active_view().file_name()

        m = None

        if filename:
            m = Utils.head_of(hsdev.client.module(file=filename))
            if not m:
                Common.show_status_message('Module {0} not found'.format(filename))
                return

        elif module_name:
            ms = []
            if scope:
                ms = hsdev.client.scope_modules(scope, input=module_name, search_type='exact')
            else:
                if self.current_file_name:
                    ms = hsdev.client.scope_modules(self.current_file_name, input=module_name, search_type='exact')
                else:
                    ms = hsdev.client.list_modules(module=module_name, db=symbols.PackageDb.from_string(db) if db else None)

            if len(ms) == 0:
                Common.show_status_message('Module {0} not found'.format(module_name))
                return
            if len(ms) == 1:
                if ms[0].by_source():
                    m = head_of(hsdev.client.module(module_name, search_type='exact', file=ms[0].location.filename))
                elif ms[0].by_cabal():
                    m = head_of(hsdev.client.module(
                        module_name,
                        search_type='exact',
                        db=ms[0].location.db,
                        package=ms[0].location.package.name))
                else:
                    m = head_of(hsdev.client.module(module_name, search_type='exact'))
            else:
                self.candidates.extend([(m, [m.name, m.location.to_string()]) for m in ms])

        else:
            if self.current_file_name:
                ms = hsdev.client.scope_modules(self.current_file_name)
            else:
                ms = hsdev.client.list_modules(db=symbols.PackageDb.from_string(db) if db else None)
            self.candidates.extend([(m, [m.name, m.location.to_string()]) for m in ms])

        if m:
            decls = list(m.declarations.values())
            self.candidates = sorted(decls, key=lambda d: d.brief())

            self.window.show_quick_panel([[decl.brief(use_unicode=False), decl.docs.splitlines()[0] if decl.docs else ''] for decl in self.candidates], self.on_symbol_selected)
            return

        self.candidates.sort(key=lambda c: c[1][0])
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


class SublimeHaskellGoToDeclaration(hsdev.HsDevTextCommand):
    def run(self, edit):
        qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])

        if Common.is_haskell_symbol_info(self.view):  # Go to within symbol info window
            loc = self.view.settings().get('location')
            if loc:
                self.view.window().open_file(loc, sublime.ENCODED_POSITION)
            else:
                Common.show_status_message('Source location of {0} not found'.format(qsymbol.name), False)
            return

        whois_name = qsymbol.qualified_name()
        full_name = qsymbol.full_name()

        current_file_name = self.view.file_name()
        # current_project = Common.get_cabal_project_dir_of_file(current_file_name)

        candidates = []
        module_candidates = []
        if not qsymbol.is_module():
            candidates = list(filter(lambda d: d.by_source(), hsdev.client.whois(whois_name, current_file_name)))

            if candidates and candidates[0].has_source_location():
                self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                return

            if candidates:
                cands = candidates[:]
                candidates = []
                for c in cands:
                    for i in c.imported:
                        candidates = [s for s in hsdev.client.symbol(input=c.name, search_type='exact', source=True) if s.module.name == i.module]
                        if candidates and candidates[0].has_source_location():
                            self.view.window().open_file(candidates[0].get_source_location(), sublime.ENCODED_POSITION)
                            return

            candidates = hsdev.client.symbol(input=qsymbol.name, search_type='exact', source=True)
        else:
            module_candidates = [m for m in hsdev.client.list_modules(source=True, module=full_name) if m.name == full_name]

        if not candidates and not module_candidates:
            Common.show_status_message('Declaration {0} not found'.format(qsymbol.name), False)
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
        self.select_candidates = [([c.brief(use_unicode=False), c.get_source_location()], True) for c in candidates] + [([m.name, m.location.filename], False) for m in module_candidates]
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
        return Common.is_haskell_source(self.view) or Common.is_haskell_repl(self.view) or (Common.is_haskell_symbol_info(self.view) and self.view.settings().get('location'))


class SublimeHaskellEvalReplaceCommand(Common.SublimeHaskellTextCommand):
    def run(self, edit, results=[]):
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
            x = json.loads(i)  # FIXME: Is it ok?
            if type(x) == str:
                return x
        except ValueError:
            return i
        return i
    return list(map(process, rs))


def ghc_eval_merge_results(l, r):
    # Prefer result in 'l', but if there's 'fail' - use result from 'r'
    return [x or y for x, y in zip(l, r)]


class SublimeHaskellEvalSelectionCommand(hsdev.HsDevTextCommand):
    def run(self, edit):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.results = ghc_eval_x(hsdev.client.ghc_eval(self.args))

        self.view.run_command('sublime_haskell_eval_replace', {
            'results': self.results})

    def is_enabled(self):
        return True

    def is_visible(self):
        return True


class SublimeHaskellApplyToSelectionCommand(hsdev.HsDevTextCommand):
    def run(self, edit):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.edit = edit
        self.view.window().show_input_panel('Function', '', self.on_done, None, self.on_cancel)

    def is_enabled(self):
        return True

    def is_visible(self):
        return True

    def on_done(self, f):
        self.results = ghc_eval_x(hsdev.client.ghc_eval(["({0}) ({1})".format(f, a) for a in self.args]))
        self.string_results = ghc_eval_x(hsdev.client.ghc_eval(["({0}) ({1})".format(f, json.dumps(a)) for a in self.args]))

        self.view.run_command('sublime_haskell_eval_replace', {
            'results': ghc_eval_merge_results(self.results, self.string_results)})

    def on_cancel(self):
        pass


class SublimeHaskellApplyToSelectionListCommand(hsdev.HsDevTextCommand):
    def run(self, edit):
        self.args = [self.view.substr(s) for s in self.view.sel()]
        self.edit = edit
        self.view.window().show_input_panel('Function', '', self.on_done, None, self.on_cancel)

    def is_enabled(self):
        return True

    def is_visible(self):
        return True

    def on_done(self, f):
        self.results = ghc_eval_x(hsdev.client.ghc_eval(['({0}) [{1}]'.format(f, ", ".join(self.args))]))
        self.string_results = ghc_eval_x(hsdev.client.ghc_eval(['({0}) [{1}]'.format(f, ", ".join([json.dumps(a) for a in self.args]))]))
        self.res = ghc_eval_merge_results(self.results, self.string_results)

        if self.res[0] is None:
            return
        else:
            result_list = json.loads(self.res[0])
            self.view.run_command('sublime_haskell_eval_replace', {'results': result_list})

    def on_cancel(self):
        pass


class AutoFixState(object):
    def __init__(self, view=None, corrections=[], selected=0, undo_history=[], redo_history=[]):
        self.view = view
        self.corrections = corrections
        self.selected = selected
        self.undo_history = undo_history[:]
        self.redo_history = redo_history[:]

    def is_active(self):
        return self.view and self.corrections

    def set(self, view, corrections, selected=0, undo_history=[], redo_history=[]):
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
        Common.output_panel(self.view.window(), 'Press {0}\n\n{1}'.format(self.keys(), self.message(cur)), 'sublime_haskell_auto_fix', syntax='HaskellAutoFix')

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
            Logging.log('AutoFixState.set_selected({0}): out of bound'.format(i), Logging.LOG_ERROR)
            return
        self.selected = i
        self.mark()

    def fix_current(self):
        self.undo_history.append((self.corrections[:], self.selected))
        self.redo_history.clear()
        (cur, corrs) = self.get_corrections()
        self.corrections = hsdev.client.autofix_fix(hsdev.encode_corrections([cur]), rest=hsdev.encode_corrections(corrs), pure=True)
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


class SublimeHaskellAutoFix(hsdev.HsDevWindowCommand):
    def run(self):
        if self.window.active_view().file_name():
            def on_resp(msgs):
                self.messages = msgs
                self.status_msg.stop()
                if msgs is None:
                    return
                sublime.set_timeout(self.on_got_messages, 0)

            def on_err(err, ds):
                self.status_msg.fail()
                self.status_msg.stop()
                Common.show_status_message('Check & Lint: {0}'.format(err), False)

            self.status_msg = Common.status_message_process('Autofix: ' + self.window.active_view().file_name(), priority=3)
            self.status_msg.start()
            hsdev.client.check_lint(files=[self.window.active_view().file_name()], ghc=Settings.get_setting_async('ghc_opts'), wait=False, on_response=on_resp, on_error=on_err, timeout=0)

    def on_got_messages(self):
        self.corrections = list(filter(lambda corr: os.path.samefile(corr.file, self.window.active_view().file_name()), hsdev.client.autofix_show(self.messages)))
        if self.corrections:
            autofix_state.set(self.window.active_view(), self.corrections)
            autofix_state.mark()


class SublimeHaskellAutoFixTextBase(Common.SublimeHaskellTextCommand):
    def is_enabled(self):
        return autofix_state.is_active() and autofix_state.view == self.view


class SublimeHaskellAutoFixWindowBase(Common.SublimeHaskellWindowCommand):
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
        self.window.active_view().run_command('sublime_haskell_auto_fix_fix_it', {'all': True})
        autofix_state.clear()


class SublimeHaskellAutoFixFix(SublimeHaskellAutoFixWindowBase):
    def run(self):
        autofix_state.unmark()
        self.window.active_view().run_command('sublime_haskell_auto_fix_fix_it')
        autofix_state.fix_current()


class SublimeHaskellAutoFixFixIt(SublimeHaskellAutoFixTextBase):
    def run(self, edit, all=False):
        corrections = autofix_state.corrections[:] if all else [autofix_state.current_correction()]
        if corrections:
            corrs = sorted(
                [(correction.to_region(self.view), correction.corrector.contents) for correction in corrections],
                key=lambda c: c[0])

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


class SublimeHaskellStackExec(sublime_plugin.TextCommand):
    """Execute a command via `stack exec`, displaying the stdout and stderr live in the SublimeHaskell output window.
    This utility command understands basic shell argument lexing, which allows quotes arounds arguments (especially needed
    when using path names containing spaces.)"""

    OUTPUT_PANEL_NAME = 'haskell_run_output'

    class SExecRunner(threading.Thread):
        def __init__(self, panel, cmdargs):
            super(SExecRunner, self).__init__()
            self.sexec_proc = OutputCollector.OutputCollector(panel, cmdargs)

        def run(self):
            self.sexec_proc.wait()

    def run(self, edit):
        win = self.view.window()
        win.show_input_panel('stack exec --', '', self.stack_exec, None, None)

    def stack_exec(self, arg):
        cmdargs = ['stack', 'exec', '--'] + shlex.split(arg)
        window = self.view.window()
        runv = Common.output_panel(window, panel_name=SublimeHaskellStackExec.OUTPUT_PANEL_NAME)
        pretty_cmdargs = 'Running \'{0}\''.format(' '.join(cmdargs))
        runv.run_command('insert', {'characters': '{0}\n{1}\n'.format(pretty_cmdargs, '-' * len(pretty_cmdargs))})

        sthread = SExecRunner(runv, cmdargs).start()

    def show_output_panel(self):
        return output_view
