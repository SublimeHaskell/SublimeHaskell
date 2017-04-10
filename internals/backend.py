# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell backend management class and helpers
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import SublimeHaskell.internals.settings as Settings

class SublimeHaskellBackend(object):
    """Base class for SublimeHaskell backends. Provides the basic interface for managing and communicating with a
    backend (hsdev, hdevtools, ghc-mod)."""

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

def initialize_backend():
    pass
    