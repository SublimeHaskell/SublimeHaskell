import threading
import time

import sublime
import sublime_plugin

import SublimeHaskell.autocomplete as Autocomplete
import SublimeHaskell.event_common as EventCommon
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.types as Types

class FlyCheckViewEventListener(sublime_plugin.ViewEventListener):
    '''The heart of fly-check support. As a view event listener, there will be an instance of this view listener
    attached to each Haskell source view.
    '''

    @classmethod
    def is_applicable(cls, settings):
        return Common.settings_has_haskell_source(settings)


    @classmethod
    def applies_to_primary_view_only(cls):
        return True


    def __init__(self, view):
        super().__init__(view)
        self.autocompleter = Autocomplete.AutoCompleter()
        self.fly_lock = threading.RLock()
        self.fly_check_loop = threading.Event()
        self.fly_check_flag = threading.Event()
        self.fly_check_thread = None
        self.next_flycheck = time.time() + Settings.PLUGIN.lint_check_fly_idle

        # They should start out as clear. Paranoia.
        self.fly_check_loop.clear()
        self.fly_check_flag.clear()


    def on_activated(self):
        if Settings.PLUGIN.lint_check_fly:
            with self.fly_lock:
                self.fly_check_thread = threading.Thread(target=self.fly_check,
                                                         name='fly-{0}'.format(self.view.file_name()))
                self.fly_check_loop.clear()
                self.fly_check_flag.clear()
                self.next_flycheck = time.time() + Settings.PLUGIN.lint_check_fly_idle

                self.fly_check_thread.start()
        else:
            self.fly_check_thread = None


    def on_deactivated(self):
        if self.fly_check_thread is not None:
            self.fly_check_loop.set()
            self.fly_check_flag.set()
            self.fly_check_thread.join()
            self.fly_check_thread = None


    def on_modified(self):
        if Settings.PLUGIN.lint_check_fly:
            with self.fly_lock:
                self.next_flycheck = time.time() + Settings.PLUGIN.lint_check_fly_idle
            self.fly_check_flag.set()


    def fly_check(self):
        tmo_event = threading.Event()
        tmo_event.clear()
        delta_t = None

        while not self.fly_check_loop.is_set():
            # Wait for the on_modified method to set the flag to let us know that there's something
            # for which we need to take action.

            if Settings.COMPONENT_DEBUG.fly_mode:
                print('fly: waiting for check flag, timeout {0}'.format(delta_t))

            self.fly_check_flag.wait(delta_t)
            if not self.fly_check_loop.is_set():
                with self.fly_lock:
                    delta_t = self.next_flycheck - time.time()

                if Settings.COMPONENT_DEBUG.fly_mode:
                    print('fly: delta_t = {0}'.format(delta_t))

                if delta_t <= 0:
                    done_check = threading.Event()
                    done_check.clear()
                    Utils.run_async('fly-check', self.do_fly, done_check)
                    done_check.wait()
                    delta_t = None

                self.fly_check_flag.clear()

    def scan_contents(self):
        current_file_name = self.view.file_name()
        view_contents = {current_file_name: self.view.substr(sublime.Region(0, self.view.size()))}

        status_msg = Common.status_message_process("Scanning {0}".format(current_file_name))
        status_msg.start()

        def scan_resp(_resp):
            status_msg.result_ok()
            _project_dir, project_name = Common.locate_cabal_project_from_view(self.view)
            self.autocompleter.drop_completions_async(current_file_name)
            self.autocompleter.generate_completions_cache(project_name, current_file_name, contents=view_contents)

        def scan_err(_err, _details):
            status_msg.result_fail()

        BackendManager.active_backend().scan(files=[current_file_name], contents=view_contents,
                                             on_response=scan_resp, on_error=scan_err)


    def do_fly(self, done_check):
        def on_done(_view):
            if done_check:
                done_check.set()

            Types.refresh_view_types(self.view)
            self.scan_contents()

        def on_error(_view):
            # Make sure to release the event, even if an error happens.
            if done_check:
                done_check.set()

        EventCommon.do_check_lint(self.view, continue_success=on_done, error_handler=on_error)
