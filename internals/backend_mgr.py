'''SublimeHaskell relies on interactions with a backend, such as *hsdev* and *ghc-mod*, to do actions such as symbol completion,
type queries and source checking. The backend manager, :py:class:`BackendManager` controls the backend's life cycle, from
intial startup to shutdown.
'''

import threading

import sublime

import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.hsdev.backend as HsDev
import SublimeHaskell.ghcmod.backend as GHCMod
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.inspector as Inspector
import SublimeHaskell.internals.utils as Utils

class BackendManager(object, metaclass=Utils.Singleton):
    '''The backend manager is a *singleton* object instantiation of this class.
    '''

    # Known backends and mapping to metadata
    BACKEND_META = {
        HsDev.HsDevBackend.backend_name(): HsDev.HsDevBackend,
        GHCMod.GHCModBackend.backend_name(): GHCMod.GHCModBackend,
        Backend.NullHaskellBackend.backend_name(): Backend.NullHaskellBackend
    }
    '''Backend name (*type* in the settings) to backend class mapping.'''

    # The manager's states:
    INITIAL = 0
    '''State in which a backend can be started via :py:meth:`intialize`. Transitions into the :py:const:`STARTUP`
    state.'''
    STARTUP = 1
    '''Indicates that a backend is being started. If successful, transitions into the :py:const:`CONNECT` state.'''
    CONNECT = 2
    '''State in which SublimeHaskell conencts to the started backend. Transitions to the :py:const:`ACTIVE` state
    if successfull.'''
    ACTIVE = 3
    '''Indicates that the backend is active and operating. Transitions to :py:const:`DISCONNECT` state when
    shutting down the backend.'''
    DISCONNECT = 4
    '''State where SublimeHaskell disconnects from the backend before shutting it down. Transitions to :py:const:`SHUTDOWN`
    state.'''
    # RESTART = 5
    SHUTDOWN = 6
    '''Indicates that SublimeHaskell is now terminating the backend, after disconnecting. Transitions to the
    :py:const:`INACTIVE` state.'''
    INACTIVE = 7
    '''Indicates that the backend was shut down and there is no backend -- SublimeHaskell has no active backend.
    There is no transition out of this state and the backend manager's state has to be manually set to :py:const:`INITIAL`
    in order to start a backend.'''

    ACTIVE_BACKEND = None
    '''The currently active backend.'''

    # Pretty-printing state support:
    STATES_TO_NAME = {
        0: 'INITIAL',
        1: 'STARTUP',
        2: 'CONNECT',
        3: 'ACTIVE',
        4: 'DISCONNECT',
        # 5: 'RESTART',
        6: 'SHUTDOWN',
        7: 'INACTIVE'
    }

    def __init__(self):
        '''Initializes the backend manager.

        .. py:attribute:: state

        The :py:class:`BackendManager`'s current state.

        .. py:attribute:: state_lock

        Recursive lock to mediate access to :py:attr:`state`.

        .. py:attribute:: action_lock

        Hard lock to serialize backend life cycle actions. This lock is acquired while initializing and shutting down
        the backend.

        .. py:attribute:: src_inspector

        The source inspector object, :py:class:`Inspector`.

        .. py:attribute:: current_backend_name

        The name of the current backend. This starts out as the default backend's name, but can be changed to a different
        backend name.

        .. py:attribute:: possible_backends

        Filtered list of backend names. This is a subset of the *backends* settings, filtered on whether the backend's *type*
        is avaiable. For example, if *hsdev* is not installed (not available), then none of the *hsdev* backend names are
        possible (and not in this list).

        .. py:attribute:: project_cache

        A mapping between project names and files associated with each project.
        '''
        super().__init__()
        BackendManager.ACTIVE_BACKEND = Backend.NullHaskellBackend(self)
        self.state = self.INITIAL
        self.state_lock = threading.RLock()
        self.action_lock = threading.Lock()
        self.src_inspector = Inspector.Inspector(BackendManager.ACTIVE_BACKEND)
        self.current_backend_name = BackendManager.ACTIVE_BACKEND.backend_name()
        self.possible_backends = {}
        self.project_cache = {}


    def __enter__(self):
        '''Top half of Python context management. This ensures that the backend is started and running when used in a
        *with* statement.
        '''
        self.initialize()
        return self


    def __exit__(self, exc_type, exc_value, exc_tb):
        '''Bottom half of Python context management. Nothing useful is done here and exceptions are not suppressed.
        '''
        return False


    def get_backends(self):
        '''Update the possible backends from settings:

        * Interrogate which backend types are available (usable backends)
        * Filter the requested backends in the *backends* setting according to availability (the backend's type is in the
          usable backends) to produce the possible backends,
        * Extract the default backend's name from the possible backends and set the current backend's name to the
          default backend.

        Updates the :py:attr:`possible_backends` and :py:attr:`current_backend_name` attributes.
        '''
        self.possible_backends = self.filter_possible(Settings.PLUGIN.backends)
        if self.possible_backends:
            # Take first available because DEFAULT_KNOWN_BACKENDS are listed in order of priority...

            print('plugin \'backends\' {0}'.format([name for name in Settings.PLUGIN.backends]))
            print('Possible/usable \'backends\': {0}'.format([name for name in self.possible_backends]))

            def_backend, n_defaults = self.get_default_backend(self.possible_backends)
            if n_defaults == 0:
                sublime.message_dialog('\n'.join(['No default backend found. Using the \'none\' backend.']))
                self.current_backend_name = Backend.NullHaskellBackend.backend_name()
            else:
                if n_defaults > 1:
                    sublime.message_dialog('Multiple default backends detected. Using {0}.'.format(def_backend))
                self.current_backend_name = def_backend
        else:
            # Yell at luser.
            self.no_backends_available()


    def no_backends_available(self):
        sublime.message_dialog('\n'.join(['No usable backends (hsdev, ghc-mod) found on PATH or',
                                          'in the standard Haskell cabal and stack installation',
                                          'locations.',
                                          '',
                                          'Please check or update your SublimeHaskell user settings,',
                                          'or install hsdev or ghc-mod.']))


    def updated_settings(self, key, _val):
        if key == 'backends':
            self.get_backends()


    def initialize(self):
        if self.current_state(self.INITIAL):
            with self.action_lock:
                # Can only start a backend iff in the INITIAL state.
                Logging.log('Starting backend \'{0}\''.format(self.current_backend_name), Logging.LOG_INFO)
                backend_info = self.possible_backends.get(self.current_backend_name, {})
                backend_clazz = self.BACKEND_META.get(backend_info.get('backend') or Backend.NullHaskellBackend.backend_name())
                if backend_clazz is not None:
                    the_backend = backend_clazz(self, **backend_info.get('options', {}))
                else:
                    the_backend = None

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


    def filter_possible(self, user_backends):
        '''Filter down user-requested backends using the available backends.
        '''
        retval = {}
        for ubname in user_backends:
            ubackend = user_backends.get(ubname)
            if ubackend is not None:
                ubclazz = BackendManager.BACKEND_META.get(ubackend.get('backend'))
                backend_options = dict(ubackend.get('options', {}))
                backend_options['backend_name'] = ubname
                if ubclazz is not None and ubclazz.is_available(**backend_options):
                    retval[ubname] = ubackend

        return retval


    def get_default_backend(self, user_backends):
        retval = None
        n_defaults = 0
        for name in user_backends:
            args = user_backends.get(name)
            if args.get('default', False):
                n_defaults = n_defaults + 1
                if retval is None:
                    retval = name

        return (retval, n_defaults)


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
            self.set_state(self.INITIAL)
            self.initialize()
            self.reassociate_all_files()


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
            the_backend = BackendManager.ACTIVE_BACKEND
            self.set_backend(Backend.NullHaskellBackend(self))
            self.state_disconnect(the_backend)
            self.state_shutdown(the_backend)
            self.state_inactive()
        finally:
            if got_lock:
                self.action_lock.release()


    def state_disconnect(self, backend):
        try:
            if self.current_state(BackendManager.ACTIVE):
                self.set_state(BackendManager.DISCONNECT)
                backend.disconnect_backend()
        except OSError:
            # Really, ignore the exceptions that should be caught and dealt with by the backend's disconnection method.
            pass


    def state_shutdown(self, backend):
        try:
            if self.current_state(BackendManager.DISCONNECT):
                self.set_state(BackendManager.SHUTDOWN)
                backend.stop_backend()
        except OSError:
            pass


    def state_inactive(self):
        if self.current_state(BackendManager.SHUTDOWN):
            # Nothing to do here, at the moment.
            self.set_state(BackendManager.INACTIVE)


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


    def is_inactive_state(self):
        with self.state_lock:
            return self.state == self.INITIAL or self.state == self.INACTIVE

    def inspector_busy(self):
        return self.src_inspector.is_busy()


    def add_project_file(self, filename, project_name, project_dir):
        ## Update the project cache if needed (does not persist...)
        if project_dir not in self.project_cache:
            self.project_cache[project_dir] = {}

        # Update our project cache, passing it on to the backend.
        cache_entry = self.project_cache[project_dir]
        if filename not in cache_entry:
            cache_entry[filename] = (1, project_name)
            # Let backend know that we're associating the project with a file...
            self.active_backend().add_project_file(filename, project_name, project_dir)

    def reassociate_all_files(self):
        for project_dir in self.project_cache:
            proj_cache = self.project_cache[project_dir]
            for project_file in proj_cache:
                proj_info = proj_cache[project_file]
                self.active_backend().add_project_file(project_file, proj_info[1], project_dir)

    @staticmethod
    def active_backend():
        '''Return the currently active backend. Note: This will return the Backend.NullHaskellBackend if there isn't an
        active backend. This ensures that API calls to the backend succeed without additional special logic.
        '''
        backend = BackendManager.ACTIVE_BACKEND
        if backend is not None and BackendManager().current_state(BackendManager.ACTIVE):
            return backend

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


def inspector_busy():
    return BackendManager().inspector_busy()


def lost_connection():
    return BackendManager().lost_connection()
