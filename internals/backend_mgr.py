"""
The backend manager.
"""

import threading

import SublimeHaskell.hsdev.backend as HsDev
import SublimeHaskell.ghcimod.backend as GHCIMod
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.logging as Logging

class BackendManager(object):
    # Known backends and mapping to metadata
    BACKEND_META = {
        'hsdev': HsDev.HsDevBackend,
        'ghc-mod': GHCIMod.GHCModBackend,
        'hdevtool': None
    }

    # The list of backends in the order in which we try to use them. Can be overridden by the
    # 'backends' preference.
    DEFAULT_BACKEND_PRIORITY = ['hsdev', 'ghc-mod', 'hdevtool']

    # The manager's states:
    INITIAL = 0
    STARTUP = 1
    CONNECT = 2
    ACTIVE = 3
    DISCONNECT = 4
    RESTART = 5
    SHUTDOWN = 6

    # The currently active backend
    ACTIVE_BACKEND = None

    # Pretty-printing state support:
    STATES_TO_NAME = {
        0: 'INITIAL',
        1: 'STARTUP',
        2: 'CONNECT',
        3: 'ACTIVE',
        4: 'DISCONNECT',
        5: 'RESTART',
        6: 'SHUTDOWN'
    }
    def __init__(self):
        super().__init__()
        self.state = BackendManager.INITIAL
        self.state_lock = threading.RLock()

    def initialize(self):
        BackendManager.ACTIVE_BACKEND = None
        self.state = BackendManager.INITIAL
        backends = Settings.PLUGIN.backends or BackendManager.DEFAULT_BACKEND_PRIORITY
        usable_backends = []

        for backend in backends:
            backend_clazz = BackendManager.BACKEND_META.get(backend)
            if backend_clazz is not None and backend_clazz.is_available():
                usable_backends.append(backend_clazz)

        if len(usable_backends) > 0:
            print('Available backends: {0}'.format(list(map(lambda clazz: clazz.backend_name(), usable_backends))))
            # Take first available because DEFAULT_KNOWN_BACKENDS are listed in order of priority...
            the_backend = usable_backends[0]()
            self.go_active(the_backend)
            with self.state_lock:
                if self.state == BackendManager.ACTIVE:
                    BackendManager.ACTIVE_BACKEND = the_backend
                elif self.state == BackendManager.INITIAL:
                    BackendManager.ACTIVE_BACKEND = None
                else:
                    state_str = BackendManager.STATES_TO_NAME.get(self.state)
                    if state_str is None:
                        state_str = self.state
                    Logging.log('BackendManager: Invalid state after go_active: {0}'.format(state_str), Logging.LOG_ERROR)
        else:
            # Yell at luser.
            print('No backends found.')

    def go_active(self, backend):
        self.set_state(BackendManager.STARTUP)
        if not backend.start_backend():
            self.set_state(BackendManager.INITIAL)
            return

        self.state = BackendManager.CONNECT
        if not backend.connect_backend():
            self.set_state(BackendManager.SHUTDOWN)
            backend.shutdown_backend()
            self.set_state(BackendManager.INITIAL)
            return

        self.set_state(BackendManager.ACTIVE)

    def set_state(self, state):
        with self.state_lock:
            self.state = state

    @staticmethod
    def active_backend():
        return BackendManager.ACTIVE_BACKEND
