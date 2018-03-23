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

        def on_new(self):
            self.do_new(self.view)

        def on_load(self):
            self.do_load(self.view)

        def on_post_save(self):
            self.do_post_save(self.view)

        def on_query_context(self, key, operator, operand, matchall):
            return self.do_query_context(self.view, key, operator, operand, matchall)

        def on_activated(self):
            self.do_activated(self.view, self.view.file_name())

        def on_modified(self):
            self.do_modified(self.view, self.view.file_name())

        def on_hover(self, point, hover_zone):
            self.do_hover(self.view, point, hover_zone)

        def on_query_completions(self, prefix, locations):
            self.do_query_completions(self.view, prefix, locations)
    else:
        def __init__(self):
            super().__init__()
            self.autocompleter = Autocomplete.AutoCompleter()
            self.backend_mgr = BackendManager.BackendManager()
            self.type_cache = Types.SourceHaskellTypeCache()

        def on_new(self, view):
            self.do_new(view)

        def on_load(self, view):
            self.do_load(view)

        def on_post_save(self, view):
            self.do_post_save(view)

        def on_query_context(self, view, key, operator, operand, matchall):
            return self.do_query_context(view, key, operator, operand, matchall)

        def on_activated(self, view):
            self.do_activated(view, view.file_name())

        def on_modified(self, view):
            self.do_modified(view, view.file_name())

        def on_hover(self, view, point, hover_zone):
            self.do_hover(view, point, hover_zone)

        def on_query_completions(self, view, prefix, locations):
            return self.do_query_completions(view, prefix, locations)


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
        if Settings.PLUGIN.use_improved_syntax and (filename.endswith(".hs") or filename.endswith(".hsc") or \
           view_settings.get('syntax', '').endswith('.tmLanguage')):
            view_settings.set('syntax', 'Packages/SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.sublime-syntax')

        EventCommon.assoc_to_project(view, self.backend_mgr, filename)
        _project_dir, project_name = Common.locate_cabal_project_from_view(view)
        Utils.run_async('rescan source {0}/{1}'.format(project_name, filename), self.rescan_source, project_name, filename,
                        {'drop_all': False})


    def do_post_save(self, view):
        if not Common.view_is_inspected_source(view):
            return

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


    def do_activated(self, view, filename):
        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.on_activated file: {1}.'.format(type(self).__name__, filename))
        if view and filename:
            Utils.run_async('on_activated', self.activated_worker, view, filename)


    def do_modified(self, _view, filename):
        if filename:
            if Settings.COMPONENT_DEBUG.event_viewer:
                print('{0} invoked.'.format(type(self).__name__ + ".on_modified"))
            self.type_cache.remove(filename)


    def do_hover(self, view, point, hover_zone):
        # Note: view.file_name() is not set in certain views, such as the "Haskell Show Types Panel". Avoid
        # generating lookup errors, which are logged in the console window (for better or worse.)
        filename = view.file_name()
        if filename and Common.view_is_haskell_source(view):
            # Ensure that we never block the Python main thread.
            info_pop = InfoPop.SublimeHaskellHoverPopup(view, view.file_name(), point, hover_zone)
            Utils.run_async('SublimeHaskellPopup.on_hover', info_pop.do_hover)


    def do_query_completions(self, view, prefix, locations):
        # Defer starting the backend until as late as possible...
        if Settings.COMPONENT_DEBUG.event_viewer or Settings.COMPONENT_DEBUG.completions:
            print('{0} invoked (prefix: {1}).'.format(type(self).__name__ + '.on_query_completions', prefix))

        completions = None
        with self.backend_mgr:
            begin_time = time.clock()

            # Only suggest symbols if the current file is part of a Cabal project.
            filename = view.file_name()
            line_contents = Common.get_line_contents(view, locations[0])
            project_name = Common.locate_cabal_project_from_view(view)[1]
            completion_flags = 0
            if not Settings.PLUGIN.add_word_completions:
                completion_flags = completion_flags | sublime.INHIBIT_WORD_COMPLETIONS
            if not Settings.PLUGIN.add_default_completions:
                completion_flags = completion_flags | sublime.INHIBIT_EXPLICIT_COMPLETIONS

            curselector = view.scope_name(locations[0])
            if Regexs.LANGUAGE_RE.search(line_contents):
                completions = self.autocompleter.get_lang_completions(project_name)
            elif Regexs.OPTIONS_GHC_RE.search(line_contents):
                completions = self.autocompleter.get_flag_completions(project_name)
            elif 'meta.import.haskell' in curselector:
                # Inside an import: Complete the imported module name:
                completions = self.autocompleter.get_import_completions(project_name, filename, locations, line_contents)
            elif 'meta.declaration.exports.haskell' in curselector:
                # Export list
                export_module = Autocomplete.EXPORT_MODULE_RE.search(line_contents)
                if export_module:
                    # qsymbol = Common.get_qualified_symbol_at_region(view, view.sel()[0])
                    # TODO: Implement
                    pass
            else:
                # Add current file's completions:
                completions = self.autocompleter.get_completions(view, locations)

            end_time = time.clock()
            if Settings.COMPONENT_DEBUG.event_viewer or Settings.COMPONENT_DEBUG.completions:
                print('time to get completions: {0} seconds'.format(end_time - begin_time))
                print('completion flag: {0}'.format(completion_flags))

        # Don't put completions with special characters (?, !, ==, etc.)
        # into completion because that wipes all default Sublime completions:
        # See http://www.sublimetext.com/forum/viewtopic.php?t=8659
        # TODO: work around this
        # comp = [c for c in completions if NO_SPECIAL_CHARS_RE.match(c[0].split('\t')[0])]
        # if Settings.PLUGIN.inhibit_completions and len(comp) != 0:
        #     return (comp, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        # return comp

        return (completions, completion_flags) # if completions else None


    def rescan_source(self, project_name, filename, drop_all=True):
        if Settings.COMPONENT_DEBUG.inspection:
            print('{0}.rescan_source: {1}/{2}'.format(type(self).__name__, project_name, filename))
        with self.backend_mgr:
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

        src_module = Utils.head_of(BackendManager.active_backend().module(file=file_shown_in_view))
        return src_module is not None and src_module.location.project is not None


    def is_scanned_source(self, view):
        file_shown_in_view = Common.window_view_and_file(view)[2]
        return file_shown_in_view is not None and \
               Utils.head_of(BackendManager.active_backend().module(file=file_shown_in_view)) is not None

    def post_successful_check(self, view):
        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.post_successful_check invoked.'.format(type(self).__name__))

        Types.refresh_view_types(view)

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0}.post_successful_check: prettify_on_save {0}'.format(Settings.PLUGIN.prettify_on_save))

        if Settings.PLUGIN.prettify_on_save:
            if Settings.PLUGIN.prettify_executable == 'stylish-haskell':
                view.run_command('sublime_haskell_stylish')
            elif Settings.PLUGIN.prettify_executable == 'hindent':
                view.run_command('sublime_haskell_hindent')


    def activated_worker(self, view, filename):
        with self.backend_mgr:
            EventCommon.assoc_to_project(view, self.backend_mgr, filename)
            _, project_name = Common.locate_cabal_project_from_view(view)
            if Common.view_is_haskell_source(view):
                self.autocompleter.generate_completions_cache(project_name, filename)


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
