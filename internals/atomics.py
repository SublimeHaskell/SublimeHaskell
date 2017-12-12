'''Atomic-access classes: generic locked objects, atomic dictionaries (ducks!) and atomic lists.
'''

import threading
from collections import UserDict, UserList


class LockedObject(object):
    def __init__(self):
        self.object_lock = threading.RLock()

    def __enter__(self):
        self.object_lock.__enter__()
        return self

    def __exit__(self, otype, value, traceback):
        self.object_lock.__exit__(otype, value, traceback)


class AtomicDuck(UserDict):
    '''Atomically locked dictionary, with the dictionary interface. Has a very silly name, completely on purpose.
    '''
    def __init__(self, contents=None):
        super().__init__(contents)
        self.dict_lock = threading.RLock()

    def __enter__(self):
        self.dict_lock.__enter__()
        return self.data

    def __exit__(self, otype, value, traceback):
        self.dict_lock.__exit__(otype, value, traceback)

class AtomicList(UserList):
    '''Atomically locked list.
    '''
    def __init__(self, contents=None):
        super().__init__(contents)
        self.list_lock = threading.RLock()

    def __enter__(self):
        self.list_lock.__enter__()
        return self.data

    def __exit__(self, otype, value, traceback):
        self.list_lock.__exit__(otype, value, traceback)
