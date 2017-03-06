# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# Miscelaneous glue, mostly for interoperability between Python2 and Python3.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

from sys import version_info

PyV3 = version_info >= (3,)

def decode_bytes(s):
    if s is None:
        return None
    return s if (PyV3 and isinstance(s, str)) else s.decode('utf-8')


def encode_bytes(s):
    if s is None:
        return None
    return s if PyV3 else s.encode('utf-8')


# unicode function
def to_unicode(s):
    return s if PyV3 else unicode(s)
