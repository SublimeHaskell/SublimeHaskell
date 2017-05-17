import threading

import sublime_plugin

import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common


class SublimeHaskellStartBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.busy = False

    def run(self):
        # Prevents the Python main thread from blocking.
        Utils.run_async(type(self).__name__ + '.do_startup', self.do_startup)

    def do_startup(self):
        with Common.status_message_process('Starting up backend', priority=1):
            try:
                self.busy = True
                BackendManager.BackendManager().initialize()
            finally:
                self.busy = False

    def is_enabled(self):
        return not self.busy and BackendManager.BackendManager().current_state(BackendManager.BackendManager.INITIAL)


class SublimeHaskellStopBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.busy = False

    def run(self):
        # Prevents the Python main thread from blocking.
        Utils.run_async(type(self).__name__ + '.do_shutdown', self.do_shutdown)

    def do_shutdown(self):
        with Common.status_message_process('Shutting down backend', priority=1):
            try:
                self.busy = True
                BackendManager.BackendManager().shutdown_backend()
            finally:
                self.busy = False

    def is_enabled(self):
        return not (self.busy or BackendManager.BackendManager().current_state(BackendManager.BackendManager.INITIAL))

class SublimeHaskellRestartBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.restart_ev = threading.Event()
        self.restart_ev.clear()

    def run(self):
        Utils.run_async('setting restart flag', self.restart_ev.set)
        Utils.run_async('backend shutdown', SublimeHaskellStopBackend(self.window).do_shutdown)
        Utils.run_async('backend startup', SublimeHaskellStartBackend(self.window).do_startup)
        Utils.run_async('clearing restart flag', self.restart_ev.clear)

    def is_enabled(self):
        return not (self.restart_ev.is_set() or \
                    BackendManager.BackendManager().current_state(BackendManager.BackendManager.INITIAL))

class SublimeHaskellChooseBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.backends = {}
        self.backend_names = []

    def run(self):
        backend_mgr = BackendManager.BackendManager()
        if len(backend_mgr.possible_backends) > 0:
            self.backend_names = [name for name in backend_mgr.possible_backends]
            self.window.show_quick_panel(self.backend_names, self.change_backend)

    def change_backend(self, idx):
        if idx >= 0:
            def start_new_backend():
                backend_name = self.backend_names[idx]
                with Common.status_message_process('Starting up \'{0}\' backend'.format(backend_name), priority=1):
                    BackendManager.BackendManager().change_current_backend(backend_name)

            Utils.run_async('change backend: startup', start_new_backend)
