import os
import time

import sublime
import sublime_plugin

import SublimeHaskell.autocomplete as Autocomplete
import SublimeHaskell.check_lint as CheckAndLint
import SublimeHaskell.info_popup as InfoPop
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.event_common as EventCommon
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
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
    BackendManager.BackendManager().shutdown_backend()


class HaskellSourceViewEventListener(EventCommon.SublimeHaskellEventCommon, sublime_plugin.ViewEventListener):
    IMPROVED_SYNTAX_FILE = 'Packages/SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.tmLanguage'
    '''The improved syntax module/file.
    '''

    @classmethod
    def is_applicable(cls, settings):
        return Common.settings_has_haskell_source(settings)


    @classmethod
    def applies_to_primary_view_only(cls):
        return False


    def on_new(self):
        return self.do_new()


    def on_load(self):
        self.do_load()

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0} Haskell source {1}.'.format(type(self).__name__ + ".on_load", self.view.file_name()))

        self.view.settings().set('translate_tabs_to_spaces', True)
        if Settings.PLUGIN.use_improved_syntax:
            name = os.path.basename(self.view.file_name().lower())
            if Settings.PLUGIN.use_improved_syntax and any([name.endswith(ext) for ext in [".hs", ".hsc"]]):
                self.view.settings().set('syntax', self.IMPROVED_SYNTAX_FILE)
            # TODO: Do we also have to fix Literate Haskell? Probably yes, but not today.


    def on_activated(self):
        filename = self.view.file_name()
        if filename:
            if Settings.COMPONENT_DEBUG.event_viewer:
                print('{0} invoked.'.format(type(self).__name__ + ".on_activated"))
            Utils.run_async('on_activated', self.activated_worker, self.view, filename)


    def on_modified(self):
        filename = self.view.file_name()
        if not filename:
            return

        if Settings.COMPONENT_DEBUG.event_viewer:
            print('{0} invoked.'.format(type(self).__name__ + ".on_modified"))

        self.type_cache.remove(filename)


    def on_post_save(self):
        self.do_post_save()


    def on_hover(self, point, hover_zone):
        # Note: view.file_name() is not set in certain views, such as the "Haskell Show Types Panel". Avoid
        # generating lookup errors, which are logged in the console window (for better or worse.)
        if self.view.file_name():
            # Ensure that we never block the Python main thread.
            info_pop = InfoPop.SublimeHaskellHoverPopup(self.view, self.view.file_name(), point, hover_zone)
            Utils.run_async('SublimeHaskellPopup.on_hover', info_pop.do_hover)


    def on_query_context(self, key, operator, operand, matchall):
        return self.do_query_context(key, operator, operand, matchall)


    def on_query_completions(self, prefix, locations):
        # Defer starting the backend until as late as possible...
        if Settings.COMPONENT_DEBUG.event_viewer or Settings.COMPONENT_DEBUG.completions:
            print('{0} invoked (prefix: {1}).'.format(type(self).__name__ + '.on_query_completions', prefix))

        completions = None
        with self.backend_mgr:
            begin_time = time.clock()

            # Only suggest symbols if the current file is part of a Cabal project.
            filename = self.view.file_name()
            line_contents = Common.get_line_contents(self.view, locations[0])
            project_name = Common.locate_cabal_project_from_view(self.view)[1]
            completion_flags = 0
            if not Settings.PLUGIN.add_word_completions:
                completion_flags = completion_flags | sublime.INHIBIT_WORD_COMPLETIONS
            if not Settings.PLUGIN.add_default_completions:
                completion_flags = completion_flags | sublime.INHIBIT_EXPLICIT_COMPLETIONS

            curselector = self.view.scope_name(locations[0])
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
                completions = self.autocompleter.get_completions(self.view, locations)

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


    def post_save_actions(self, filename):
        self.type_cache.remove(filename)

        # Invoke methods directly (surely, there is a better way) to ensure that they are actually executed, even if the
        # source inspector is active. If the inspector is active, that will effectively make the SublimeText commands
        # appear to be disabled.

        # auto build enabled and file within a cabal project
        if Settings.PLUGIN.enable_auto_build:
            self.view.window().run_command('sublime_haskell_build_auto')
        elif Settings.PLUGIN.enable_auto_check and Settings.PLUGIN.enable_auto_lint:
            CheckAndLint.exec_check_and_lint_process(self.view)
            Types.refresh_view_types(self.view)
        elif Settings.PLUGIN.enable_auto_check:
            CheckAndLint.exec_check_process(self.view)
            Types.refresh_view_types(self.view)
        elif Settings.PLUGIN.enable_auto_lint:
            CheckAndLint.exec_lint_process(self.view)

        if Settings.PLUGIN.prettify_on_save:
            if Settings.PLUGIN.prettify_executable == 'stylish-haskell':
                self.view.run_command('sublime_haskell_stylish')
            elif Settings.PLUGIN.prettify_executable == 'hindent':
                self.view.run_command('sublime_haskell_hindent')


class CabalSourceViewEventListener(EventCommon.SublimeHaskellEventCommon, sublime_plugin.ViewEventListener):
    @classmethod
    def is_applicable(cls, settings):
        return Common.settings_has_cabal_source(settings)


    @classmethod
    def applies_to_primary_view_only(cls):
        return False


    def on_new(self):
        return self.do_new()


    def on_load(self):
        return self.do_load()


    def on_post_save(self):
        self.do_post_save()


    def on_query_context(self, key, operator, operand, matchall):
        return self.do_query_context(key, operator, operand, matchall)
