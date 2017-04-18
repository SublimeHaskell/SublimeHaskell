"""
The `hsdev` backend
"""

import re

import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.which as Which

class HsDevBackend(Backend.HaskellBackend):
    """This class encapsulates all of the functions that interact with the `hsdev` backend.
    """

    HSDEV_DEFAULT_PORT = 4567
    HSDEV_MIN_VER = [0, 2, 0, 0]  # minimum hsdev version
    HSDEV_MAX_VER = [0, 2, 3, 0]  # maximum hsdev version

    def __init__(self):
        super().__init__()

    @staticmethod
    def backend_name():
        return 'hsdev'

    @staticmethod
    def is_available():
        return Which.which('hsdev', ProcHelper.ProcHelper.get_extended_env().get('PATH'))

    def start_backend(self):
        return True

    def connect_backend(self):
        return True

    def hsdev_version(self):
        retval = None
        exit_code, out, _ = ProcHelper.ProcHelper.run_process(['hsdev', 'version'])

        if exit_code == 0:
            hsver = re.match(r'(?P<major>\d+)\.(?P<minor>\d+)\.(?P<revision>\d+)\.(?P<build>\d+)', out)
            if hsver:
                major = int(hsver.group('major'))
                minor = int(hsver.group('minor'))
                revision = int(hsver.group('revision'))
                build = int(hsver.group('build'))
                retval = [major, minor, revision, build]

        return retval
