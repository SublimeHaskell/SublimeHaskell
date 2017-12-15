import threading
import time

import sublime
import sublime_plugin

import SublimeHaskell.check_lint as CheckLint
import SublimeHaskell.event_common as EventCommon
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.sublime_haskell_common as Common


class FlyCheckViewEventListener(EventCommon.SublimeHaskellEventCommon, sublime_plugin.ViewEventListener):
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
        self.fly_lock = threading.RLock()
        self.fly_check_loop = threading.Event()
        self.fly_check_flag = threading.Event()
        self.fly_check_thread = None
        self.next_flycheck = time.time()+ Settings.PLUGIN.lint_check_fly_idle

        # They should start out as clear. Paranoia.
        self.fly_check_loop.clear()
        self.fly_check_flag.clear()


    def on_activated(self):
        if Settings.PLUGIN.lint_check_fly:
            with self.fly_lock:
                self.fly_check_thread = threading.Thread(target=self.fly_check)
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
                    ## Do the flycheck...
                    auto_check_enabled = Settings.PLUGIN.enable_auto_check
                    auto_lint_enabled = Settings.PLUGIN.enable_auto_lint
                    sublime.set_timeout(lambda: self.scan_contents(self.view), 0)

                    if auto_check_enabled and auto_lint_enabled:
                        check_cmd = 'sublime_haskell_check_and_lint'
                    elif auto_check_enabled:
                        check_cmd = 'sublime_haskell_check'
                    elif auto_lint_enabled:
                        check_cmd = 'sublime_haskell_lint'
                    else:
                        check_cmd = None

                    if Settings.COMPONENT_DEBUG.fly_mode:
                        print('fly: executing {0}'.format(check_cmd))

                    if check_cmd:
                        CheckLint.SublimeHaskellHsDevChain.reset_chain_flag()
                        self.view.run_command(check_cmd, {'fly': True})
                        if Settings.COMPONENT_DEBUG.fly_mode:
                            print('fly: awaiting command completion')
                        CheckLint.SublimeHaskellHsDevChain.run_chain_flag().wait()

                    delta_t = None

                self.fly_check_flag.clear()

    def scan_contents(self, view):
        current_file_name = view.file_name()
        status_msg = Common.status_message_process("Scanning {0}".format(current_file_name), priority=3)
        status_msg.start()

        def scan_resp(_resp):
            status_msg.result_ok()
            self.update_completions_async([current_file_name])

        def scan_err(_err, _details):
            status_msg.result_fail()

        view_contents = {current_file_name: view.substr(sublime.Region(0, view.size()))}
        BackendManager.active_backend().scan(contents=view_contents, on_response=scan_resp, on_error=scan_err)
