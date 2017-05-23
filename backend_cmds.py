import threading

import sublime_plugin

import SublimeHaskell.internals.backend_mgr as BackendManager
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
        backend_mgr = BackendManager.BackendManager()
        with Common.status_message_process('Starting up {0} backend'.format(backend_mgr.current_backend_name), priority=1):
            try:
                self.busy = True
                backend_mgr.set_state(BackendManager.BackendManager.INITIAL)
                backend_mgr.initialize()
            finally:
                self.busy = False

    def is_enabled(self):
        return not self.busy and BackendManager.BackendManager().is_inactive_state()


class SublimeHaskellStopBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.busy = False

    def run(self):
        # Prevents the Python main thread from blocking.
        Utils.run_async(type(self).__name__ + '.do_shutdown', self.do_shutdown)

    def do_shutdown(self):
        backend_mgr = BackendManager.BackendManager()
        with Common.status_message_process('Shutting down {0} backend'.format(backend_mgr.current_backend_name), priority=1):
            try:
                self.busy = True
                backend_mgr.shutdown_backend()
            finally:
                self.busy = False

    def is_enabled(self):
        return not (self.busy or BackendManager.BackendManager().is_inactive_state())


class SublimeHaskellRestartBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.restart_ev = threading.Event()
        self.restart_ev.clear()

    def run(self):
        Utils.run_async('restarting backend', self.do_restart)

    def is_enabled(self):
        return not (self.restart_ev.is_set() or BackendManager.BackendManager().is_inactive_state())

    def do_restart(self):
        self.restart_ev.set()
        try:
            SublimeHaskellStopBackend(self.window).do_shutdown()
            SublimeHaskellStartBackend(self.window).do_startup()
        finally:
            self.restart_ev.clear()

class SublimeHaskellChooseBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.backends = {}
        self.backend_names = []

    def run(self):
        backend_mgr = BackendManager.BackendManager()
        if len(backend_mgr.possible_backends) > 0:
            self.backend_names = [name for name in backend_mgr.possible_backends]
            self.backend_names.sort()
            self.window.show_quick_panel(self.backend_names, self.change_backend)

    def change_backend(self, idx):
        if idx >= 0:
            def start_new_backend():
                backend_name = self.backend_names[idx]
                with Common.status_message_process('Changing backend to \'{0}\' backend'.format(backend_name), priority=2):
                    BackendManager.BackendManager().change_current_backend(backend_name)

            Utils.run_async('change backend: startup', start_new_backend)
