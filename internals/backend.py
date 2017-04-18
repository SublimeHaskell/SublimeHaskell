# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell backend management class and helpers
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

class HaskellBackend(object):
    '''Base class for SublimeHaskell backends. Provides the basic interface for managing and communicating with a
    backend (hsdev, hdevtools, ghc-mod).
    '''

    def __init__(self):
        pass

    @staticmethod
    def backend_name():
        '''Return the backend's name, e.g. `hsdev` or `ghc-mod`.
        '''
        return "Default null Haskell backend"

    @staticmethod
    def is_availabile():
        '''Test if the backend is available to use
        '''
        return False

    def start_backend(self):
        '''This method allows the `HaskellBackend` subclass to start a local process, if needed, with which it interacts.
        `hsdev`'s local backend option takes advantage of it.

        Returns `True` if backend startup was successful. If no local external process is created, just return 'True'.

        The `HaskellBackend` base class returns `False` to return the backend manager's state to BackendManager.INITIAL.
        You can't start up something that doesn't exist.
        '''
        return False

    def connect_backend(self):
        '''Once the backend has been started, this method is where the `HaskellBackend` subclass creates a connection to it.

        Returns 'True' if connected successfully to the backend.
        '''
        return False

    def disconnect_backend(self):
        pass

    def stop_backend(self):
        pass

    def valid_version(self):
        return True
