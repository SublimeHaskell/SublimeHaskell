"""
The backend manager.
"""

import threading

import sublime

import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.hsdev.backend as HsDev
import SublimeHaskell.ghcimod.backend as GHCIMod
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.inspector as Inspector
import SublimeHaskell.internals.utils as Utils

class BackendManager(object, metaclass=Utils.Singleton):
    # Known backends and mapping to metadata
    BACKEND_META = {
        HsDev.HsDevBackend.backend_name(): HsDev.HsDevBackend,
        GHCIMod.GHCModBackend.backend_name(): GHCIMod.GHCModBackend,
        Backend.NullHaskellBackend.backend_name(): Backend.NullHaskellBackend
    }

    # The list of backends in the order in which we try to use them. Can be overridden by the
    # 'backends' preference.
    DEFAULT_BACKEND_PRIORITY = [
        HsDev.HsDevBackend.backend_name(),
        GHCIMod.GHCModBackend.backend_name(),
        Backend.NullHaskellBackend.backend_name()
    ]

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

    # Current state:

    def __init__(self):
        super().__init__()
        self.state = BackendManager.INITIAL
        self.state_lock = threading.RLock()
        self.src_inspector = None

    def initialize(self):
        BackendManager.ACTIVE_BACKEND = None
        self.state = BackendManager.INITIAL

        usable_backends = self.available_backends()
        if len(usable_backends) > 0:
            print('Available backends: {0}'.format(list(map(lambda clazz: clazz.backend_name(), usable_backends))))
            # Take first available because DEFAULT_KNOWN_BACKENDS are listed in order of priority...
            the_backend = usable_backends[0](self)
            self.go_active(the_backend)
            with self.state_lock:
                if self.current_state(BackendManager.ACTIVE):
                    BackendManager.ACTIVE_BACKEND = the_backend
                elif self.current_state(BackendManager.INITIAL):
                    BackendManager.ACTIVE_BACKEND = Backend.NullHaskellBackend(self)
                else:
                    state_str = BackendManager.STATES_TO_NAME.get(self.state, str(self.state))
                    Logging.log('BackendManager: Invalid state after go_active: {0}'.format(state_str), Logging.LOG_ERROR)
        else:
            # Yell at luser.
            print('No backends found.')

    def available_backends(self):
        usable_backends = []

        for backend in Settings.PLUGIN.backends or BackendManager.DEFAULT_BACKEND_PRIORITY:
            backend_clazz = BackendManager.BACKEND_META.get(backend)
            if backend_clazz is not None and backend_clazz.is_available():
                usable_backends.append(backend_clazz)

        return usable_backends

    def go_active(self, backend):
        '''Walk through the state phases (INITIAL -> STARTUP -> CONNECT -> ACTIVE) to startup and connect a backend.
        '''
        if self.current_state(BackendManager.INITIAL):
            self.set_state(BackendManager.STARTUP)
            if not backend.start_backend():
                self.set_state(BackendManager.INITIAL)
                return

        # Not sure how the code would do anything but transition from INITIAL to STARTUP (i.e., come into the function
        # in the STARTUP state...)
        if self.current_state(BackendManager.STARTUP):
            self.set_state(BackendManager.CONNECT)
            if not backend.connect_backend():
                self.set_state(BackendManager.SHUTDOWN)
                backend.shutdown_backend()
                self.set_state(BackendManager.INITIAL)
                return

        if self.current_state(BackendManager.CONNECT):
            # Start the source inspector
            with self.state_lock:
                self.src_inspector = Inspector.Inspector(backend)
                self.src_inspector.start()
                Logging.log('Inspector started.', Logging.LOG_DEBUG)

                self.src_inspector.start_inspect()
            self.set_state(BackendManager.ACTIVE)

    def shutdown_backend(self):
        '''Step through the backend shutdown process: disconnect (if active), stop backend (if disconnected). Backend
        manager's state should end up in INITIAL.
        '''
        backend = BackendManager.ACTIVE_BACKEND
        if self.current_state(BackendManager.ACTIVE):
            self.set_state(BackendManager.DISCONNECT)
            backend.disconnect_backend()

        if self.current_state(BackendManager.DISCONNECT):
            self.set_state(BackendManager.SHUTDOWN)
            # Ask the source inspector to terminate -- shutting down the backend takes longer, which means we'll spend
            # less time in join()
            self.src_inspector.terminate()
            backend.stop_backend()
            while self.src_inspector.is_alive():
                self.src_inspector.join(1.500)
            self.src_inspector = None

        if self.current_state(BackendManager.SHUTDOWN):
            # Paranoia: If we're shut down, assume no backend... :-)
            BackendManager.ACTIVE_BACKEND = Backend.NullHaskellBackend(self)
            self.set_state(BackendManager.INITIAL)

    def lost_connection(self):
        '''Shut down the backend due to a lost connection, such as a reset socket. Backend's state should end up in
        INITIAL.
        '''
        sublime.error_message('\n'.join(['SublimeHaskell: Support backend abruptly disconnected.',
                                         '',
                                         'To restart the backend, invoke:',
                                         '',
                                         '    SublimeHaskell: Start backend',
                                         ''
                                         'from the SublimeText command palette.']))
        self.shutdown_backend()

    def set_state(self, state):
        with self.state_lock:
            self.state = state

    def current_state(self, state):
        '''Test if the current backend state is equal to `state`, with the assurance that this method has the lock on
        the backend manager object.
        '''
        with self.state_lock:
            return self.state == state

    @staticmethod
    def active_backend():
        '''Return the currently active backend. Note: This will return the Backend.NullHaskellBackend if there isn't an
        active backend. This ensures that API calls to the backend succeed without additional special logic.
        '''
        backend = BackendManager.ACTIVE_BACKEND
        if backend is not None and BackendManager().current_state(BackendManager.ACTIVE):
            return backend
        else:
            return Backend.NullHaskellBackend(BackendManager())

    @staticmethod
    def is_live_backend():
        '''Determine if the active backend is live and usable. The null backend is never live.
        '''
        return BackendManager.active_backend().is_live_backend()


    @staticmethod
    def inspector():
        '''Get the source inspector object.
        '''
        return BackendManager().src_inspector


def active_backend():
    '''Return the active backend. This is a convenience function that accesses the `BackendManager`'s static
    function of the same name and reduces the amount of redundant typing.
    '''
    return BackendManager.active_backend()

def is_live_backend():
    '''Determine if the active backend is live and usable. This is a convenience function that accesses the `BackendManager`'s
    static function of the same name and reduces the amount of redundant typing.
    '''
    return BackendManager.is_live_backend()

def inspector():
    '''Return the inspector object. This is a convenience function that accesses the `BackendManager`'s static
    function of the same name, reducing the amount of redundant typing.
    '''
    return BackendManager.inspector()

def lost_connection():
    return BackendManager().lost_connection()
