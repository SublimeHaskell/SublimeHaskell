import SublimeHaskell.backend_cmds as BackendCmds
import SublimeHaskell.autocomplete as Autocomplete
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.types as Types
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.settings as Settings


class SublimeHaskellEventCommon(object):
    '''A class whose methods are common across :py:class:`sublime_plugin.ViewEventListener` instances.
    :py:class:`sublime_plugin.ViewEventListener` does not play nicely with multiple inheritance (or single inheritance,
    for that matter). This class contains all of the methods that a base class would implement, if the base class'
    :py:method:`__init__` were eventually called.
    '''

    def __init__(self, view):
        super().__init__(view)
        self.view = view
        self.autocompleter = Autocomplete.AutoCompleter()
        self.backend_mgr = BackendManager.BackendManager()
        self.type_cache = Types.SourceHaskellTypeCache()

    COMPLETION_CHARS = {'is_module_completion': '.',
                        'is_import_completion': '('}

    def context_haskell_autofix(self, _key, _operator, _operand, _matchall):
        return self.view.settings().get('autofix', False)

    def context_auto_completion_popup(self, _key, _operator, _operand, _matchall):
        return Settings.PLUGIN.auto_completion_popup

    def context_haskell_source(self, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_source(self.view)

    def context_haskell_source_or_repl(self, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_source(self.view) or Common.view_is_haskell_repl(self.view)

    def context_haskell_repl(self, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_repl(self.view)

    def context_haskell_symbol_info(self, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_symbol_info(self.view)

    def context_cabal_source(self, _key, _operator, _operand, _matchall):
        return Common.view_is_cabal_source(self.view)

    def context_scanned_source(self, _key, _operator, _operand, _matchall):
        return self.is_scanned_source()

    def context_in_project(self, _key, _operator, _operand, _matchall):
        return self.is_in_project()

    def context_completion(self, key, _operator, _operand, _matchall):
        # Completion context is the only branch here where a backend is needed.
        retval = False
        with self.backend_mgr:
            project_dir, project_name = Common.locate_cabal_project_from_view(self.view)
            region = self.view.sel()[0]
            if region.a == region.b:
                word_region = self.view.word(region)
                preline = Common.get_line_contents_before_region(self.view, word_region)
                preline += self.COMPLETION_CHARS[key]
                qsymbol = Common.get_qualified_symbol(preline)
                if qsymbol.module:
                    mod_completions = self.autocompleter.get_current_module_completions(project_name, project_dir)
                    if qsymbol.is_import_list:
                        retval = qsymbol.module in mod_completions
                    else:
                        retval = [m for m in mod_completions if m.startswith(qsymbol.module)] != []

        return retval


    CONTEXT_DISPATCH = {'haskell_autofix': context_haskell_autofix,
                        'auto_completion_popup': context_auto_completion_popup,
                        'haskell_source': context_haskell_source,
                        'haskell_source_or_repl': context_haskell_source_or_repl,
                        'haskell_repl': context_haskell_repl,
                        'haskell_symbol_info': context_haskell_symbol_info,
                        'cabal_source': context_cabal_source,
                        'scanned_source': context_scanned_source,
                        'in_project': context_in_project,
                        'is_module_completion': context_completion,
                        'is_import_completion': context_completion}
    '''Context-to-function dispatch dictionary. Used in py:method:`on_qeuery_context` to reduce the branch overhead
    incurred by testing against each key.
    '''

    def do_new(self):
        filename = self.view.file_name()
        if not filename:
            return

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0} invoked.'.format(type(self).__name__ + ".on_new"))

        self.assoc_to_project(self.view, filename)
        _project_dir, project_name = Common.locate_cabal_project_from_view(self.view)
        Utils.run_async('rescan {0}/{1}'.format(project_name, filename), self.rescan_source, project_name, filename,
                        {'drop_all': True})
        self.view.settings().set('translate_tabs_to_spaces', True)


    def do_load(self):
        filename = self.view.file_name()
        if filename is None:
            return

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0} {1}.'.format(type(self).__name__ + ".on_load", filename))

        self.assoc_to_project(self.view, filename)
        _project_dir, project_name = Common.locate_cabal_project_from_view(self.view)
        Utils.run_async('rescan source {0}/{1}'.format(project_name, filename), self.rescan_source, project_name, filename,
                        {'drop_all': False})


    def do_post_save(self):
        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0} invoked.'.format(type(self).__name__ + ".on_post_save"))

        filename = self.view.file_name()
        if not filename:
            if Settings.COMPONENT_DEBUG.event_viewer:
                print('{0}: no file name.'.format(type(self).__name__ + ".on_post_save"))
            return

        self.post_save_actions(filename)

        _project_dir, project_name = Common.locate_cabal_project_from_view(self.view)
        Utils.run_async('rescan source {0}/{1}'.format(project_name, filename), self.rescan_source, project_name, filename,
                        {'drop_all': False})


    def do_query_context(self, key, operator, operand, matchall):
        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0} key = {1}.'.format(type(self).__name__ + '.on_query_context', key))

        dispatch = self.CONTEXT_DISPATCH.get(key)
        if dispatch:
            ## print('on_query_context: {0}({1}, {2}, {3}, {4})'.format(dispatch.__name__, key, operator, operand, matchall))
            return dispatch(self, key, operator, operand, matchall)

        return None


    def is_in_project(self):
        file_shown_in_view = Common.window_view_and_file(self.view)[2]
        if file_shown_in_view is None:
            return False

        src_module = Utils.head_of(BackendManager.active_backend().module(file=file_shown_in_view))
        return src_module is not None and src_module.location.project is not None


    def is_scanned_source(self):
        file_shown_in_view = Common.window_view_and_file(self.view)[2]
        return file_shown_in_view is not None and \
               Utils.head_of(BackendManager.active_backend().module(file=file_shown_in_view)) is not None


    def assoc_to_project(self, view, filename):
        ## Update file -> project tracking
        project_dir, project_name = Common.locate_cabal_project(filename)
        if project_dir and project_name:
            self.backend_mgr.add_project_file(filename, project_name, project_dir)
        BackendCmds.cabal_project_status(view, self.backend_mgr)


    def activated_worker(self, view, filename):
        with self.backend_mgr:
            self.assoc_to_project(view, filename)
            _, project_name = Common.locate_cabal_project_from_view(view)
            if Common.view_is_haskell_source(view):
                self.autocompleter.generate_completions_cache(project_name, filename)


    def post_save_actions(self, filename):
        pass


    def rescan_source(self, project_name, filename, drop_all=True):
        with self.backend_mgr:
            with self.backend_mgr.inspector() as insp:
                insp.mark_file_dirty(filename)

            self.update_completions_async(project_name, [filename], drop_all)


    def update_completions_async(self, project_name, files=None, drop_all=False):
        if drop_all:
            Utils.run_async('drop all completions', self.autocompleter.drop_completions_async)
        else:
            for file in files or []:
                Utils.run_async('{0}: drop completions'.format(file), self.autocompleter.drop_completions_async, file)

        for file in files or []:
            Utils.run_async('{0}: init completions'.format(file), self.autocompleter.generate_completions_cache,
                            project_name, file)
