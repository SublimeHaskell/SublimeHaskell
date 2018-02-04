import re

import sublime

import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.symbols as Symbols


class SublimeHaskellInsertImportForSymbol(CommandWin.BackendTextCommand):
    '''Insert import for symbol. Uses the advanced feature API in the backend.
    '''
    def __init__(self, view):
        super().__init__(view)
        self.candidates = None
        self.backend = Backend.NullHaskellBackend(BackendManager.BackendManager())

    def run(self, edit, **args):
        kw_module = args.get('module')
        self.backend = BackendManager.active_backend()

        if not kw_module:
            kw_decl = args.get('decl')
            if kw_decl is None:
                qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])
                kw_decl = qsymbol.qualified_name()

            current_file_name = self.view.file_name()

            # Phase 1: Get the candidate import modules: the backend's query_import returns the (flag, list) tuple.
            # If successful (flag == True), then invoke add_import to add the import to the module's existing
            # modules.
            (status, self.candidates) = self.backend.query_import(kw_decl, current_file_name)

            if status:
                self.unique_candidates = set(self.candidates)
                self.select_candidate()
            else:
                if len(self.candidates) == 1:
                    Common.sublime_status_message(self.candidates[0])
                else:
                    sublime.message_dialog('\n'.join(self.candidates))
        else:
            self.add_import(edit, kw_module)

    def select_candidate(self):
        self.unique_candidates = sorted(set(self.candidates))
        if len(self.unique_candidates) == 1:
            self.on_selected_candidate(0)
        else:
            self.view.window().show_quick_panel([[c.module.name + ': ' + c.brief()] for c in self.unique_candidates], self.on_selected_candidate)

    def on_selected_candidate(self, idx):
        if idx >= 0:
            def proj_or_pkg(c):
                proj = Symbols.location_project(c)
                pkg = Symbols.location_package(c)
                return proj or (pkg.name if pkg is not None else None)

            selected_candidate = self.unique_candidates[idx]
            self.candidates = sorted(
                filter(lambda c: c == selected_candidate, self.candidates),
                key=lambda c: (proj_or_pkg(c.imported_from.location) != proj_or_pkg(c.module.location), c.imported_from.name),
            )
            self.select_import()

    def select_import(self):
        if len(self.candidates) == 1:
            self.on_selected_import(0)
        else:
            self.view.window().show_quick_panel([[c.imported_from.name, str(c.imported_from.location)] for c in self.candidates], self.on_selected_import)

    def on_selected_import(self, idx):
        if idx >= 0:
            # By this point, the `run` method has exited, so the `edit` is no longer valid. Reinvoke the command
            # with the module name so that a fresh `edit` is created.
            self.view.run_command('sublime_haskell_insert_import_for_symbol', {'module': self.candidates[idx].imported_from.name})

    def add_import(self, edit, module_name):
        contents = self.view.substr(sublime.Region(0, self.view.size()))

        # Phase 2: Ask the backend to turn the contents into a list of Module objects:
        imp_module = self.backend.contents_to_module(self.view.file_name(), contents)
        if imp_module is not None:
            imports = sorted(imp_module.imports, key=lambda i: i.position.line)
            after = [imp for imp in imports if imp.module > module_name]

            insert_line = 0
            insert_gap = False

            if after:
                # Insert before after[0]
                insert_line = after[0].position.line - 1
            elif imports:
                # Insert after all imports
                insert_line = imports[-1].position.line
            else:
                declarations = self.backend.symbol(file=self.view.file_name())
                if declarations:
                    # Insert before first declaration
                    # HOWTO: Detect signature?
                    insert_line = min([d.position.line for d in declarations]) - 2
                    insert_gap = True
                else:
                    # Try to add the import just after the "where" of the module declaration
                    contents = self.view.substr(sublime.Region(0, self.view.size()))
                    mod_decl = re.search('module.*where', contents, re.MULTILINE)
                    if mod_decl is not None:
                        insert_line = self.view.rowcol(mod_decl.end())[0]
                        insert_gap = True
                    else:
                        # Punt! Insert at the end of the file
                        insert_line = self.view.rowcol(self.view.size())[0]

            insert_text = 'import {0}\n'.format(module_name) + ('\n' if insert_gap else '')

            point = self.view.text_point(insert_line, 0)
            self.view.insert(edit, point, insert_text)

            Common.sublime_status_message('Import {0} added'.format(module_name))

    def is_visible(self):
        return Common.view_is_haskell_source(self.view) and super().is_visible()
