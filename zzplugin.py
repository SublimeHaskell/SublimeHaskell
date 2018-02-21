import os
import time

import sublime
import sublime_plugin

import SublimeHaskell.autocomplete as Autocomplete
import SublimeHaskell.build as Builder
import SublimeHaskell.info_popup as InfoPop
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.event_common as EventCommon
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.types as Types
import SublimeHaskell.internals.regexes as Regexs


def plugin_loaded():
    '''All of the good stuff that happens when SublimeHaskell is loaded.
    '''
    cache_path = Common.sublime_haskell_cache_path()
    backend_mgr = BackendManager.BackendManager()

    if not os.path.exists(cache_path):
        os.makedirs(cache_path)

    # Probably already loaded... doesn't hurt to reload. Refresh the backend manager's list of backends, while we're at it.
    Settings.load_settings()
    backend_mgr.get_backends()

    # Register change detection:
    Settings.PLUGIN.add_change_callback('add_to_PATH', ProcHelper.ProcHelper.update_environment)
    Settings.PLUGIN.add_change_callback('add_standard_dirs', ProcHelper.ProcHelper.update_environment)
    Settings.PLUGIN.add_change_callback('backends', backend_mgr.updated_settings)


def plugin_unloaded():
    '''Finalization actions when SublimeHaskell is unloaded.
    '''
    ParseOutput.MARKER_MANAGER.clear_error_marks()
    BackendManager.BackendManager().shutdown_backend()

HAS_VIEWEV_API = int(sublime.version()) >= 3155
if HAS_VIEWEV_API:
    EV_SUBCLASS = sublime_plugin.ViewEventListener
else:
    EV_SUBCLASS = sublime_plugin.EventListener


class SublimeHaskellEventListener(EV_SUBCLASS):
    COMPLETION_CHARS = {'is_module_completion': '.',
                        'is_import_completion': '('}

    def context_haskell_autofix(self, view, _key, _operator, _operand, _matchall):
        return view.settings().get('autofix', False)

    def context_auto_completion_popup(self, _view, _key, _operator, _operand, _matchall):
        return Settings.PLUGIN.auto_completion_popup

    def context_haskell_source(self, view, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_source(view)

    def context_haskell_source_or_repl(self, view, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_source(view) or Common.view_is_haskell_repl(view)

    def context_haskell_repl(self, view, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_repl(view)

    def context_haskell_symbol_info(self, view, _key, _operator, _operand, _matchall):
        return Common.view_is_haskell_symbol_info(view)

    def context_cabal_source(self, view, _key, _operator, _operand, _matchall):
        return Common.view_is_cabal_source(view)

    def context_scanned_source(self, view, _key, _operator, _operand, _matchall):
        return self.is_scanned_source(view)

    def context_in_project(self, view, _key, _operator, _operand, _matchall):
        return self.is_in_project(view)

    def context_completion(self, view, key, _operator, _operand, _matchall):
        # Completion context is the only branch here where a backend is needed.
        retval = False
        with self.backend_mgr:
            project_dir, project_name = Common.locate_cabal_project_from_view(view)
            region = view.sel()[0]
            if region.a == region.b:
                word_region = view.word(region)
                preline = Common.get_line_contents_before_region(view, word_region)
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

    if HAS_VIEWEV_API:
        @classmethod
        def is_applicable(cls, settings):
            return Common.settings_has_haskell_source(settings)

        @classmethod
        def applies_to_primary_view_only(cls):
            return False

        ## I hate this duplication! But the ST3 developers changed the ViewEventListener and EventListener APIs
        ## within a major version.
        def __init__(self, view):
            super().__init__(view)
            self.autocompleter = Autocomplete.AutoCompleter()
            self.backend_mgr = BackendManager.BackendManager()
            self.type_cache = Types.SourceHaskellTypeCache()
            # I get two calls of `on_post_save` after saving file
            # To prevent extra calls to check/infer etc., we store here last updated time
            # view => (view.update_count(), time.clock())
            self.update_cache = {}

        def on_new(self):
            self.do_new(self.view)

        def on_load(self):
            self.do_load(self.view)

        def on_post_save(self):
            self.do_post_save(self.view)

        def on_query_context(self, key, operator, operand, matchall):
            self.do_query_context(self.view, key, operator, operand, matchall)
    else:
        def __init__(self):
            super().__init__()
            self.autocompleter = Autocomplete.AutoCompleter()
            self.backend_mgr = BackendManager.BackendManager()
            self.type_cache = Types.SourceHaskellTypeCache()
            # I get two calls of `on_post_save` after saving file
            # To prevent extra calls to check/infer etc., we store here last updated time
            # view => (view.update_count(), time.clock())
            self.update_cache = {}

        def on_new(self, view):
            self.do_new(view)

        def on_load(self, view):
            self.do_load(view)

        def on_post_save(self, view):
            self.do_post_save(view)

        def on_query_context(self, view, key, operator, operand, matchall):
            self.do_query_context(view, key, operator, operand, matchall)

    def do_new(self, view):
        filename = view.file_name()
        if not Common.view_is_haskell_source(view) or not filename:
            return

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.on_new invoked.'.format(type(self).__name__))

        EventCommon.assoc_to_project(view, self.backend_mgr, filename)
        _project_dir, project_name = Common.locate_cabal_project_from_view(view)
        Utils.run_async('rescan {0}/{1}'.format(project_name, filename), self.rescan_source, project_name, filename,
                        {'drop_all': True})
        view.settings().set('translate_tabs_to_spaces', True)


    def do_load(self, view):
        filename = view.file_name()
        if not Common.view_is_haskell_source(view) or not filename:
            return

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.on_load {1}.'.format(type(self).__name__, filename))

        view_settings = view.settings() or {}
        if (Settings.PLUGIN.use_improved_syntax and (name.endswith(".hs") or name.endswith(".hsc"))) or \
           view_settings.get('syntax', '').endswith('.tmLanguage'):
            view_settings.set('syntax', 'Packages/SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.sublime-syntax')

        EventCommon.assoc_to_project(view, self.backend_mgr, filename)
        _project_dir, project_name = Common.locate_cabal_project_from_view(view)

        if Settings.PLUGIN.enable_infer_types:
            BackendManager.active_backend().infer(files=[filename])

        Utils.run_async('rescan source {0}/{1}'.format(project_name, filename), self.rescan_source, project_name, filename,
                        {'drop_all': False})


    def do_post_save(self, view):
        if not Common.view_is_inspected_source(view):
            return

        current_time = time.clock()
        last_update = self.update_cache.get(view.file_name())
        if last_update is not None and last_update[0] == view.change_count() and (current_time - last_update[1]) < 0.2:
            # view contents equals
            # and last update was in less then 0.2s before, skipping
            print('SublimeHaskellEventListener: duplicate save detected.')
            return
        self.update_cache[view.file_name()] = (view.change_count(), current_time)

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.on_post_save invoked.'.format(type(self).__name__))

        filename = view.file_name()
        if not filename:
            if Settings.COMPONENT_DEBUG.event_viewer:
                print('{0}.on_post_save: no file name.'.format(type(self).__name__))
            return

        _project_dir, project_name = Common.locate_cabal_project_from_view(view)
        if Common.view_is_haskell_source(view):
            self.type_cache.remove(filename)

            if Settings.PLUGIN.enable_auto_build:
                Builder.Builder(view, continue_success=self.post_successful_check).auto_build()
            else:
                EventCommon.do_check_lint(view, continue_success=self.post_successful_check)

        Utils.run_async('rescan source {0}/{1}'.format(project_name, filename), self.rescan_source, project_name,
                        filename, False)

    def do_query_context(self, view, key, operator, operand, matchall):
        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.on_query_context key = {1}.'.format(type(self).__name__, key))

        dispatch = self.CONTEXT_DISPATCH.get(key)
        if dispatch:
            ## print('on_query_context: {0}({1}, {2}, {3}, {4})'.format(dispatch.__name__, key, operator, operand, matchall))
            return dispatch(self, view, key, operator, operand, matchall)

        return None


    def rescan_source(self, project_name, filename, drop_all=True):
        if Settings.COMPONENT_DEBUG.inspection:
            print('{0}.rescan_source: {1}/{2}'.format(type(self).__name__, project_name, filename))
        with self.backend_mgr:
            if self.backend_mgr.active_backend().auto_rescan():
                if Utils.head_of(BackendManager.active_backend().module(None, file=filename, header=True)) is not None:
                    return
            with self.backend_mgr.inspector() as insp:
                if not filename.endswith('.cabal'):
                    insp.mark_file_dirty(filename)
                else:
                    insp.mark_cabal_dirty(filename)

        EventCommon.update_completions_async(self.autocompleter, project_name, [filename], drop_all)


    def is_in_project(self, view):
        file_shown_in_view = Common.window_view_and_file(view)[2]
        if file_shown_in_view is None:
            return False

        src_module = Utils.head_of(BackendManager.active_backend().module(None, file=file_shown_in_view, header=True))
        return src_module is not None and src_module.location.project is not None


    def is_scanned_source(self, view):
        file_shown_in_view = Common.window_view_and_file(view)[2]
        return file_shown_in_view is not None and \
               Utils.head_of(BackendManager.active_backend().module(None, file=file_shown_in_view, header=True)) is not None

    def post_successful_check(self, view):
        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.post_successful_check invoked.'.format(type(self).__name__))

        if Settings.PLUGIN.enable_infer_types:
            BackendManager.active_backend().infer(files=[view.file_name()])

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.post_successful_check: prettify_on_save {0}'.format(Settings.PLUGIN.prettify_on_save))

        if Settings.PLUGIN.prettify_on_save:
            if Settings.PLUGIN.prettify_executable == 'stylish-haskell':
                view.run_command('sublime_haskell_stylish')
            elif Settings.PLUGIN.prettify_executable == 'hindent':
                view.run_command('sublime_haskell_hindent')

## Not needed at present.
##
# class CabalSourceViewEventListener(sublime_plugin.ViewEventListener):
#     @classmethod
#     def is_applicable(cls, settings):
#         return Common.settings_has_cabal_source(settings)
#
#
#     @classmethod
#     def applies_to_primary_view_only(cls):
#         return False
