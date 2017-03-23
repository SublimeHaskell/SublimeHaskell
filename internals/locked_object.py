# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell backend management class and helpers
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import threading

class LockedObject(object):
    """A general-purpose object with locking semantics. This is designed to be used via the 'with' statement:

    x = LockedObject(initial_value)
    with x as v:
        # Do something with v...
        pass
    """

    def __init__(self, obj, lock=None):
        self.object_lock = lock if lock else threading.RLock()
        self.object = obj

    def __enter__(self):
        self.object_lock.__enter__()
        return self.object

    def __exit__(self, otype, value, traceback):
        self.object_lock.__exit__(otype, value, traceback)
