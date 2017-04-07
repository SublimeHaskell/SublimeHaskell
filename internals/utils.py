"""
Miscelaneous glue, mostly for interoperability between Python2 and Python3.
"""

import os

def decode_bytes(src):
    return src.decode('utf-8').replace('\r\n', '\n').replace('\r', '\n') if src is not None else None


def encode_bytes(src):
    return src.replace('\r\n', os.linesep).replace('\n', os.linesep).encode('utf-8') if src is not None else None


def head_of(lst):
    return lst[0] if lst is not None and len(lst) > 0 else None


def tool_enabled(feature):
    """Generate the name of a feature to test whether it is enabled."""
    return 'enable_' + str(feature)
