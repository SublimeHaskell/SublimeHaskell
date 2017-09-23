import re

import sublime

import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.cmdwin_types as CommandWin


class SublimeHaskellInsertImportForSymbol(CommandWin.BackendTextCommand):
    '''Insert import for symbol. Uses the advanced feature API in the backend.
    '''
    def __init__(self, view):
        super().__init__(view)
        self.edit = None
        self.candidates = None
        self.cand_idx = 0
        self.backend = Backend.NullHaskellBackend(BackendManager.BackendManager())

    def run(self, edit, **kwargs):
        decl = kwargs.get('decl')
        self.backend = BackendManager.active_backend()

        full_name = decl
        if full_name is None:
            qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.sel()[0])
            full_name = qsymbol.qualified_name()

        current_file_name = self.view.file_name()
        self.edit = edit

        # Phase 1: Get the candidate import modules: the backend's query_import returns the (flag, list) tuple.
        # If successful (flag == True), then invoke add_import to add the import to the module's existing
        # modules.
        (status, self.candidates) = self.backend.query_import(full_name, current_file_name)
        if status:
            if len(self.candidates) == 1:
                self.add_import(self.candidates[0].module.name)
            else:
                self.view.window().show_quick_panel([[c.module.name] for c in self.candidates], self.on_done)
                if self.cand_idx >= 0:
                    self.add_import(self.candidates[self.cand_idx].module.name)
        else:
            if len(self.candidates) == 1:
                Common.show_status_message(self.candidates[0])
            else:
                sublime.message_dialog('\n'.join(self.candidates))

    def on_done(self, idx):
        self.cand_idx = idx

    def add_import(self, module_name):
        contents = self.view.substr(sublime.Region(0, self.view.size()))

        # Truncate contents to the module declaration and the imports list, if present.
        imports_list = list(re.finditer('^import.*$', contents, re.MULTILINE))
        if len(imports_list) > 0:
            contents = contents[0:imports_list[-1].end()]

        # Phase 2: Ask the backend to turn the contents into a list of Module objects:
        imp_module = self.backend.contents_to_module(contents)
        if imp_module is not None:
            imports = sorted(imp_module.imports, key=lambda i: i.position.line)
            after = [imp for imp in imports if imp.module > module_name]

            insert_line = 0
            insert_gap = False

            if len(after) > 0:
                # Insert before after[0]
                insert_line = after[0].position.line - 1
            elif len(imports) > 0:
                # Insert after all imports
                insert_line = imports[-1].position.line
            elif len(imp_module.declarations) > 0:
                # Insert before first declaration
                insert_line = min([d.position.line for d in imp_module.declarations.values()]) - 1
                insert_gap = True
            else:
                # Try to add the import just after the "where" of the module declaration
                contents = self.view.substr(sublime.Region(0, self.view.size()))
                mod_decl = re.search('module.*where', contents, re.MULTILINE)
                if mod_decl is not None:
                    insert_line = mod_decl.end()
                    insert_gap = True
                else:
                    # Punt! Insert at the end of the file
                    insert_line = self.view.rowcol(self.view.size())[0]

            insert_text = 'import {0}\n'.format(module_name) + ('\n' if insert_gap else '')

            point = self.view.text_point(insert_line, 0)
            self.view.insert(self.edit, point, insert_text)

            Common.show_status_message('Import {0} added'.format(module_name), True)

    def is_visible(self):
        return Common.is_haskell_source(self.view) and super().is_visible()
