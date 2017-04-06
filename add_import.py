import json
import re

import pprint

import sublime

import SublimeHaskell.hsdev.agent as hsdev
import SublimeHaskell.hsdev.result_parse as HsDevResultParse
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common


class SublimeHaskellInsertImportForSymbol(hsdev.HsDevTextCommand):
    """
    Insert import for symbol
    """
    def __init__(self, view):
        super().__init__(view)
        self.full_name = None
        self.current_file_name = None
        self.edit = None
        self.candidates = None
        self.module_name = None

    def run(self, edit, **kwargs):
        filename = kwargs.get('filename')
        decl = kwargs.get('decl')
        module_name = kwargs.get('module_name')

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
        else:
            self.candidates = hsdev.client.lookup(self.full_name, self.current_file_name)

            if not self.candidates:
                Common.show_status_message('Symbol {0} not found'.format(self.full_name))
            elif len(self.candidates) == 1:
                self.add_import(self.candidates[0].module.name)
            else:
                self.view.window().show_quick_panel([[c.module.name] for c in self.candidates], self.on_done)

    def add_import(self, module_name):
        self.module_name = module_name
        contents = self.view.substr(sublime.Region(0, self.view.size()))
        # Truncate contents to the module declaration and the imports list, if present.
        imports_list = list(re.finditer('^import.*$', contents, re.MULTILINE))
        if len(imports_list) > 0:
            imports_list = imports_list[-1].end()
            contents = contents[0:imports_list]

        ProcHelper.ProcHelper.invoke_tool(['hsinspect'], 'hsinspect', contents, self.on_inspected, check_enabled=False)

    def on_inspected(self, result):
        imp_module = None

        if self.view.is_dirty():
            # Use the buffer's contents
            pyresult = json.loads(result).get('result')
            if pyresult is not None:
                if Logging.is_log_level(Logging.LOG_DEBUG):
                    pprint.pprint(pyresult, width=80)
                modinfo = pyresult.get('module')
                if modinfo is not None:
                    imp_module = HsDevResultParse.parse_module(modinfo)
        else:
            # Otherwise, use the actual file
            imp_module = Utils.head_of(hsdev.client.module(file=self.current_file_name))

        if imp_module is not None:
            imports = sorted(imp_module.imports, key=lambda i: i.position.line)
            after = [i for i in imports if i.module > self.module_name]

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

            insert_text = 'import {0}\n'.format(self.module_name) + ('\n' if insert_gap else '')

            point = self.view.text_point(insert_line, 0)
            self.view.insert(self.edit, point, insert_text)

            Common.show_status_message('Import {0} added'.format(self.module_name), True)

    def on_done(self, idx):
        if idx >= 0:
            self.view.run_command('sublime_haskell_insert_import_for_symbol',
                                  {'filename': self.current_file_name,
                                   'module_name': self.candidates[idx].module.name})

    def is_visible(self):
        return Common.is_haskell_source(self.view)
