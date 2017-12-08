import threading

import sublime
import sublime_plugin

import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.settings as Settings


class SublimeHaskellStartBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.busy = False

    def run(self, **_args):
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
                cabal_project_status(self.window.active_view(), BackendManager.BackendManager())

    def is_enabled(self):
        return not self.busy and BackendManager.BackendManager().is_inactive_state()


class SublimeHaskellStopBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.busy = False

    def run(self, **_args):
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
                cabal_project_status(self.window.active_view(), BackendManager.BackendManager())

    def is_enabled(self):
        return not (self.busy or BackendManager.BackendManager().is_inactive_state())


class SublimeHaskellRestartBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.restart_ev = threading.Event()
        self.restart_ev.clear()

    def run(self, **_args):
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
            cabal_project_status(self.window.active_view(), BackendManager.BackendManager())

class SublimeHaskellChooseBackend(sublime_plugin.WindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.backends = {}
        self.backend_names = []

    def run(self, **_args):
        backend_mgr = BackendManager.BackendManager()

        # Rescan for backends to ensure we have the most up-to-date list...
        backend_mgr.possible_backends = backend_mgr.filter_possible(Settings.PLUGIN.backends)
        if backend_mgr.possible_backends:
            print('plugin \'backends\' {0}'.format([name for name in Settings.PLUGIN.backends]))
            print('Possible/usable \'backends\': {0}'.format([name for name in backend_mgr.possible_backends]))

            if len(backend_mgr.possible_backends) > 1:
                self.backend_names = [name for name in backend_mgr.possible_backends]
                self.backend_names.sort()
                self.window.show_quick_panel(self.backend_names, self.change_backend)
            elif len(backend_mgr.possible_backends) == 1:
                backend_name = list(backend_mgr.possible_backends)[0]
                sublime.message_dialog('Only one backend, \'{0}\', available -- starting it.'.format(backend_name))
                self.start_new_backend(backend_name)
        else:
            backend_mgr.no_backends_available()

    def change_backend(self, idx):
        if idx >= 0:
            Utils.run_async('change backend: startup', self.start_new_backend, self.backend_names[idx])


    def start_new_backend(self, backend_name):
        with Common.status_message_process('Changing backend to \'{0}\''.format(backend_name), priority=2):
            BackendManager.BackendManager().change_current_backend(backend_name)
            cabal_project_status(self.window.active_view(), BackendManager.BackendManager())

def cabal_project_status(view, backend_mgr):
    vsettings = view.settings()
    project_name = vsettings.get(Settings.SETTING_SUBHASK_PROJECT)
    if project_name is None:
        project_name = '_unknown_'

    active_backend = backend_mgr.active_backend()
    view.set_status('sublime_haskell_cabal', 'cabal: {0} [{1}]'.format(project_name, active_backend.backend_name()))
