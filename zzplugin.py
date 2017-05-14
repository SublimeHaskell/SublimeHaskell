
import os
import threading
import time

import sublime
import sublime_plugin

import SublimeHaskell.autocomplete as Autocomplete
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.types as Types


def plugin_loaded():
    '''All of the good stuff that happens when SublimeHaskell is loaded.
    '''
    cache_path = Common.sublime_haskell_cache_path()

    if not os.path.exists(cache_path):
        os.makedirs(cache_path)

    # Probably already loaded... doesn't hurt to reload.
    Settings.load_settings()

    # Register change detection:
    Settings.PLUGIN.add_change_callback('add_to_PATH', ProcHelper.ProcHelper.update_environment)
    Settings.PLUGIN.add_change_callback('add_standard_dirs', ProcHelper.ProcHelper.update_environment)

    # Create the backend, but don't initialize yet.
    backend = BackendManager.BackendManager()
    backend.initialize()


def plugin_unloaded():
    '''Finalization actions when SublimeHaskell is unloaded.
    '''
    BackendManager.BackendManager().shutdown_backend()


class SublimeHaskellEventListener(sublime_plugin.EventListener):
    '''The plugin's primary SublimeText event listener, consolidating actions related to file I/O (post-save actions,
    buffer modifications and flycheck linting.) It is also a good place to localize the plugin's various singletons.
    '''
    def __init__(self):
        super().__init__()
        self.project_file_name = ''
        self.autocompleter = Autocomplete.AutoCompleter()
        # Fly mode state:
        self.fly_view = LockedObject.LockedObject({'view':None, 'mtime':None})
        self.fly_event = threading.Event()
        self.fly_agent = threading.Thread(target='fly_check')


    def on_new(self, view):
        self.set_cabal_status(view)
        if Common.is_inspected_source(view):
            filename = view.file_name()
            if filename:
                BackendManager.inspector().mark_file_dirty(filename)
                self.update_completions_async(drop_all=True)


    def on_load(self, view):
        filename = view.file_name()
        if filename:
            if Settings.PLUGIN.use_improved_syntax:
                name = os.path.basename(filename.lower())
                if name.endswith(".hs") or name.endswith(".hsc"):
                    view.settings().set('syntax', 'Packages/SublimeHaskell/Syntaxes/Haskell-SublimeHaskell.tmLanguage')
                # TODO Do we also have to fix Literate Haskell?

            self.set_cabal_status(view)
            if Common.is_inspected_source(view):
                BackendManager.inspector().mark_file_dirty(filename)
                self.update_completions_async(drop_all=True)

    def on_post_save(self, view):
        filename = view.file_name()
        if filename:
            if Common.is_inspected_source(view):
                BackendManager.inspector().mark_file_dirty(filename)
                self.update_completions_async(drop_all=True)
            if Common.is_haskell_source(view):
                Types.SourceHaskellTypeCache().remove(filename)
                self.trigger_build(view)

    def on_modified(self, view):
        if Common.is_haskell_source(view):
            if Settings.PLUGIN.lint_check_fly and view.file_name():
                self.fly(view)
            Types.SourceHaskellTypeCache().remove(view.file_name())

    def on_activated(self, view):
        self.set_cabal_status(view)
        window = view.window()
        if window:
            if not self.project_file_name:
                self.project_file_name = window.project_file_name()
            if window.project_file_name() is not None and window.project_file_name() != self.project_file_name:
                self.project_file_name = window.project_file_name()
                Logging.log('project switched to {0}, reinspecting'.format(self.project_file_name))
                Logging.log('reinspect all', Logging.LOG_TRACE)
                BackendManager.active_backend().remove_all()
                BackendManager.inspector().start_inspect()
                BackendManager.inspector().force_inspect()

    def on_query_context(self, view, key, _operator, _operand, _matchall):
        retval = None
        if key == 'haskell_autofix':
            retval = view.settings().get('autofix')
        elif key == 'auto_completion_popup':
            retval = Settings.PLUGIN.auto_completion_popup
        elif key == 'haskell_source':
            retval = Common.is_haskell_source(view)
        elif key == 'haskell_source_or_repl':
            retval = Common.is_haskell_source(view) or Common.is_haskell_repl(view)
        elif key == 'haskell_repl':
            retval = Common.is_haskell_repl(view)
        elif key == 'haskell_symbol_info':
            retval = Common.is_haskell_symbol_info(view)
        elif key == 'cabal_source':
            retval = Common.is_cabal_source(view)
        elif key == 'scanned_source':
            retval = self.is_scanned_source(view)
        elif key == 'in_project':
            retval = self.is_in_project(view)
        elif key == "is_module_completion" or key == "is_import_completion":
            chars = {
                "is_module_completion": '.',
                "is_import_completion": '('}

            region = view.sel()[0]
            if region.a != region.b:
                retval = False
            else:
                word_region = view.word(region)
                preline = Common.get_line_contents_before_region(view, word_region)
                preline += chars[key]
                retval = self.can_complete_qualified_symbol(Common.get_qualified_symbol(preline))

        return retval

    def on_query_completions(self, view, _prefix, locations):
        if not Common.is_haskell_source(view):
            return []

        begin_time = time.clock()

        # Only suggest symbols if the current file is part of a Cabal project.
        line_contents = Common.get_line_contents(view, locations[0])
        completions = self.autocompleter.get_import_completions(view, locations, line_contents)
        completions = completions + self.autocompleter.get_special_completions(line_contents)

        # Export list
        if 'meta.declaration.exports.haskell' in view.scope_name(view.sel()[0].a):
            export_module = Autocomplete.EXPORT_MODULE_RE.search(line_contents)
            if export_module:
                # qsymbol = Common.get_qualified_symbol_at_region(view, view.sel()[0])
                # TODO: Implement
                pass

        # Add current file's completions:
        completions = completions + self.autocompleter.get_completions(view, locations)
        Autocomplete.sort_completions(completions)

        end_time = time.clock()
        Logging.log('time to get completions: {0} seconds'.format(end_time - begin_time), Logging.LOG_DEBUG)

        # Don't put completions with special characters (?, !, ==, etc.)
        # into completion because that wipes all default Sublime completions:
        # See http://www.sublimetext.com/forum/viewtopic.php?t=8659
        # TODO: work around this
        # comp = [c for c in completions if NO_SPECIAL_CHARS_RE.match(c[0].split('\t')[0])]
        # if Settings.PLUGIN.inhibit_completions and len(comp) != 0:
        #     return (comp, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        # return comp

        if Settings.PLUGIN.inhibit_completions:
            if len(completions) > 0:
                return (completions, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
            else:
                # Because we want to inhibit completions...
                return []
        else:
            return completions

    def on_activated_async(self, view):
        filename = view.file_name()
        if Common.is_haskell_source(view) and filename:
            Utils.run_async('get completions for {0}'.format(filename),
                            self.autocompleter.get_completions_async, filename)


    def set_cabal_status(self, view):
        filename = view.file_name()
        if filename:
            # If directory is important: (dir, project_name) = Common.get_cabal_project_dir_and_name_of_file(filename)
            project_name = Common.get_cabal_project_dir_and_name_of_file(filename)[1]
            if project_name:
                # TODO: Set some useful status instead of this
                view.set_status('sublime_haskell_cabal', '{0}: {1}'.format('cabal', project_name))


    def trigger_build(self, view):
        cabal_project_dir, _ = Common.get_cabal_project_dir_and_name_of_view(view)

        # don't flycheck
        self.nofly()

        # auto build enabled and file within a cabal project
        if Settings.PLUGIN.enable_auto_build and cabal_project_dir is not None:
            view.window().run_command('sublime_haskell_build_auto')
        elif Settings.PLUGIN.enable_auto_check and Settings.PLUGIN.enable_auto_lint:
            view.window().run_command('sublime_haskell_check_and_lint')
            view.window().run_command('sublime_haskell_get_types')
        elif Settings.PLUGIN.enable_auto_check:
            view.window().run_command('sublime_haskell_check')
            view.window().run_command('sublime_haskell_get_types')
        elif Settings.PLUGIN.enable_auto_lint:
            view.window().run_command('sublime_haskell_lint')


    def can_complete_qualified_symbol(self, info):
        '''Helper function, returns whether sublime_haskell_complete can run for (module, symbol, is_import_list)
        '''
        if not info.module:
            return False

        if info.is_import_list:
            return info.module in self.autocompleter.get_current_module_completions()
        else:
            return list(filter(lambda m: m.startswith(info.module), self.autocompleter.get_current_module_completions())) != []


    def update_completions_async(self, files=None, drop_all=False):
        if drop_all:
            Utils.run_async('drop all completions', self.autocompleter.drop_completions_async)
        else:
            for file in files or []:
                Utils.run_async('drop completions', self.autocompleter.drop_completions_async, file)
        Utils.run_async('init completions', self.autocompleter.init_completions_async)


    def is_scanned_source(self, view):
        _, view, file_shown_in_view = Common.get_haskell_command_window_view_file_project(view)
        if file_shown_in_view is None:
            return False
        return Utils.head_of(BackendManager.active_backend().module(file=file_shown_in_view)) is not None


    def is_in_project(self, view):
        _, view, file_shown_in_view = Common.get_haskell_command_window_view_file_project(view)
        if file_shown_in_view is None:
            return False
        else:
            src_module = Utils.head_of(BackendManager.active_backend().module(file=file_shown_in_view))
            return src_module is not None and src_module.location.project is not None


    def fly(self, view):
        with self.fly_view as view:
            view['view'] = view
            view['mtime'] = time.time()
        self.fly_event.set()

    def nofly(self):
        with self.fly_view as view:
            view['view'] = None
            view['mtime'] = None
        self.fly_event.set()

    def fly_check(self):
        while True:
            view_ = None
            mtime_ = None
            delay = Settings.PLUGIN.lint_check_fly_idle

            with self.fly_view as view:
                view_ = view['view']
                mtime_ = view['mtime']

            if not view_:  # Wait for signal
                self.fly_event.wait()
                self.fly_event.clear()
                time.sleep(delay)
                continue

            if time.time() - mtime_ < delay:  # Was modified recently, sleep more
                time.sleep(delay)
                continue
            else:
                with self.fly_view as view:
                    view['view'] = None
                    view['mtime'] = None

                auto_check_enabled = Settings.PLUGIN.enable_auto_check
                auto_lint_enabled = Settings.PLUGIN.enable_auto_lint
                sublime.set_timeout(lambda: self.scan_contents(view_), 0)

                fly_window = view_.window()
                if auto_check_enabled and auto_lint_enabled:
                    sublime.set_timeout(lambda: fly_window.run_command('sublime_haskell_check_and_lint', {'fly': True}), 0)
                elif auto_check_enabled:
                    sublime.set_timeout(lambda: fly_window.run_command('sublime_haskell_check', {'fly': True}), 0)
                elif auto_lint_enabled:
                    sublime.set_timeout(lambda: fly_window.run_command('sublime_haskell_lint', {'fly': True}), 0)

    def scan_contents(self, view):
        current_file_name = view.file_name()
        status_msg = Common.status_message_process("Scanning {0}".format(current_file_name), priority=3)
        status_msg.start()

        def scan_resp(_resp):
            status_msg.stop()
            self.update_completions_async([current_file_name])

        def scan_err(_err, _details):
            status_msg.fail()
            status_msg.stop()

        scan_contents = {current_file_name: view.substr(sublime.Region(0, view.size()))}
        BackendManager.active_backend().scan(contents=scan_contents, on_response=scan_resp, on_error=scan_err)
