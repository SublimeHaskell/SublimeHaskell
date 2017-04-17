# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell backend management class and helpers
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.internals.which as Which

class HaskellBackend(object):
    '''Base class for SublimeHaskell backends. Provides the basic interface for managing and communicating with a
    backend (hsdev, hdevtools, ghc-mod).'''

    def __init__(self):
        pass

    def activate_backend(self):
        pass

    def inactivate_backend(self):
        pass

    def start_backend(self):
        pass

    def stop_backend(self):
        pass

    def valid_version(self):
        return True


class BackendManager(object):
    # Known backends. Can be overridden by the 'backends' preference.
    DEFAULT_KNOWN_BACKENDS = ['hsdev', 'ghc-mod', 'hdevtools']

    # The manager's states:
    INITIAL = 0

    def __init__(self):
        super().__init__()
        self.state = BackendManager.INITIAL

    def initialize(self):
        backends = Settings.PLUGIN.backends or BackendManager.DEFAULT_KNOWN_BACKENDS
        env_path = ProcHelper.ProcHelper.get_extended_env().get('PATH')
        avail = list(filter(lambda b: Which.which([b], env_path) is not None, backends))
        if avail:
            print('Available backends: {0}'.format(avail))
            # Take first available because DEFAULT_KNOWN_BACKENDS are listed in order of priority...
            the_backend = avail[0]
        else:
            # Yell at luser.
            print('No backends found.')
