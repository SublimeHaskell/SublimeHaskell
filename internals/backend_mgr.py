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
        BackendManager.ACTIVE_BACKEND = Backend.NullHaskellBackend(self)
        self.state = self.INITIAL
        self.state_lock = threading.RLock()
        self.action_lock = threading.Lock()
        self.src_inspector = Inspector.Inspector(BackendManager.ACTIVE_BACKEND)
        self.current_backend_name = BackendManager.ACTIVE_BACKEND.backend_name()
        self.possible_backends = {}

        self.get_backends()


    def __enter__(self):
        self.initialize()
        return self


    def __exit__(self, exc_type, exc_value, exc_tb):
        return False


    def get_backends(self):
        '''Update the possible backends from settings: interrogate which backend types are available, then
        filter the requested backends according to availability.
        '''
        usable_backends = self.available_backends()
        if len(usable_backends) > 0:
            # Take first available because DEFAULT_KNOWN_BACKENDS are listed in order of priority...
            self.possible_backends = self.down_select(Settings.PLUGIN.backends, usable_backends)

            print('Available backends: {0}'.format([clazz.backend_name() for clazz in usable_backends]))
            print('plugin \'backends\' {0}'.format([name for name in Settings.PLUGIN.backends]))
            print('Possible/usable backends: {0}'.format([name for name in self.possible_backends]))

            def_backend, n_defaults = self.get_default_backend(self.possible_backends)
            if n_defaults == 0:
                sublime.message_dialog('No default backend found. Proceeding without a backend.')
            else:
                if n_defaults > 1:
                    sublime.message_dialog('Multiple default backends detected. Using {0}.'.format(def_backend))
                self.current_backend_name = def_backend
        else:
            # Yell at luser.
            sublime.message_dialog('\n'.join(['No usable backends (hsdev, ghc-mod) found on PATH or',
                                              'in the standard Haskell cabal and stack installation',
                                              'locations.',
                                              '',
                                              'Please check or update your SublimeHaskell user settings,',
                                              'or install hsdev or ghc-mod.',
                                              '',
                                              'Proceeding without a backend.']))


    def initialize(self):
        if self.current_state(self.INITIAL):
            with self.action_lock:
                # Can only start a backend iff in the INITIAL state.
                Logging.log('Starting backend \'{0}\''.format(self.current_backend_name), Logging.LOG_INFO)
                backend_info = self.possible_backends[self.current_backend_name]
                the_backend = self.make_backend(backend_info.get('backend'), backend_info.get('options', {}))

                if the_backend is not None:
                    self.state_startup(the_backend)
                    self.state_connect(the_backend)
                    self.state_active(the_backend)

                    if self.current_state(self.INITIAL):
                        # Something failed during startup, revert to the null backend
                        self.set_backend(Backend.NullHaskellBackend(self))
                    elif not self.current_state(self.ACTIVE):
                        state_str = self.STATES_TO_NAME.get(self.state, str(self.state))
                        Logging.log('BackendManager: Invalid state after starting backend: {0}'.format(state_str),
                                    Logging.LOG_ERROR)


    def available_backends(self):
        '''Determine which backends are actually available. Whether a backend is available is backend-specific, usually the
        backend class examines the PATH environment variable and looks for a specific executable.
        '''
        return [clazz for clazz in BackendManager.BACKEND_META.values() if clazz.is_available()]


    def down_select(self, user_backends, avail_backends):
        '''Filter down user-requested backends using the available backends.
        '''
        backend_names = [b.backend_name() for b in avail_backends]
        return dict([(name, user_backends[name]) for name in user_backends
                     if user_backends[name].get('backend', '') in backend_names])


    def get_default_backend(self, user_backends):
        retval = None
        n_defaults = 0
        for name in user_backends:
            args = user_backends.get(name)
            if args.get('default', False):
                n_defaults = n_defaults + 1
                if retval is None:
                    retval = name
        if retval == '':
            # Huh. No default backend?
            retval = None

        return (retval, n_defaults)


    def make_backend(self, backend_name, options):
        backend_clazz = self.BACKEND_META.get(backend_name, None)
        if backend_clazz is not None:
            return backend_clazz(self, **options)
        else:
            return None


    def set_backend(self, new_backend):
        BackendManager.ACTIVE_BACKEND = new_backend
        with Inspector.Inspector(new_backend) as insp:
            self.src_inspector = insp
            insp.start_inspect()


    def change_current_backend(self, new_backend_name):
        the_backend = self.possible_backends.get(new_backend_name, None)
        if the_backend is not None:
            self.shutdown_backend(get_action_lock=True)
            self.current_backend_name = new_backend_name
            self.initialize()


    def state_startup(self, backend):
        if self.current_state(self.INITIAL):
            successful_startup = False
            try:
                self.set_state(self.STARTUP)
                successful_startup = backend.start_backend()
            finally:
                if not successful_startup:
                    self.set_state(self.INITIAL)


    def state_connect(self, backend):
        if self.current_state(self.STARTUP):
            # Not sure how the code would do anything but transition from INITIAL to STARTUP (i.e., come into the
            # function in the STARTUP state...)
            successful_connect = False
            try:
                self.set_state(self.CONNECT)
                successful_connect = backend.connect_backend()
            finally:
                if not successful_connect:
                    try:
                        self.set_state(BackendManager.SHUTDOWN)
                        self.shutdown_backend(get_action_lock=False)
                    finally:
                        self.set_state(self.INITIAL)


    def state_active(self, backend):
        if self.current_state(self.CONNECT):
            self.set_state(BackendManager.ACTIVE)
            self.set_backend(backend)


    def shutdown_backend(self, get_action_lock=True):
        '''Step through the backend shutdown process: disconnect (if active), stop backend (if disconnected). Backend
        manager's state should end up in INITIAL.
        '''
        # If the action lock was previously acquired, don't try to re-acquire it.
        got_lock = get_action_lock and self.action_lock.acquire()
        try:
            self.state_disconnect()
            self.state_shutdown()
            self.state_reset_initial()
        finally:
            if got_lock:
                self.action_lock.release()


    def state_disconnect(self):
        try:
            if self.current_state(BackendManager.ACTIVE):
                self.set_state(BackendManager.DISCONNECT)
                BackendManager.ACTIVE_BACKEND.disconnect_backend()
        except OSError:
            # Really, ignore the exceptions that should be caught and dealt with by the backend's disconnection method.
            pass


    def state_shutdown(self):
        try:
            if self.current_state(BackendManager.DISCONNECT):
                self.set_state(BackendManager.SHUTDOWN)
                BackendManager.ACTIVE_BACKEND.stop_backend()
        except OSError:
            pass


    def state_reset_initial(self):
        if self.current_state(BackendManager.SHUTDOWN):
            # Paranoia: If we're shut down, assume no backend... :-)
            self.set_backend(Backend.NullHaskellBackend(self))
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
        self.shutdown_backend(get_action_lock=True)


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
