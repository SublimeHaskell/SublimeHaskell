# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# Miscelaneous glue, mostly for interoperability between Python2 and Python3.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import os

def decode_bytes(s):
    return s.decode('utf-8').replace(os.linesep, '\n') if s is not None else None


def encode_bytes(s):
    return s or s.replace('\n', os.linesep).encode('utf-8') if s is not None else None


# unicode function
def to_unicode(s):
    return s


def head_of(l):
    if l is None or not len(l):
        return None
    return l[0]


def tool_enabled(feature):
    return 'enable_{0}'.format(feature)

## Used for singleton objects, like the status manager.
class Singleton(type):
    _instances = {}
    def __call__(cls, *args, **kwargs):
        print('cls.instance {0}'.format(cls._instances))
        if cls not in cls._instances:
            cls._instances[cls] = super(Singleton, cls).__call__(*args, **kwargs)
        # else:
        #     cls._instances[cls].__init__(*args, **kwargs)
        return cls._instances[cls]
