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
        self.object_lock = threading.RLock() if not lock else lock
        self.internal_obj = obj

    def __enter__(self):
        self.object_lock.__enter__()
        return self.internal_obj

    def __exit__(self, otype, value, traceback):
        self.object_lock.__exit__(otype, value, traceback)

    def set(self, value):
        with self.object_lock:
            self.internal_obj = value
