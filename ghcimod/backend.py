"""
The ghc-mod backend
"""

import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.which as Which

class GHCModBackend(Backend.HaskellBackend):
    """This class encapsulates all of the functions that interact with the `hsdev` backend.
    """
    def __init__(self, backend_mgr):
        super().__init__(backend_mgr)

    @staticmethod
    def backend_name():
        return 'ghc-mod'

    @staticmethod
    def is_available():
        return Which.which('ghc-mod', ProcHelper.ProcHelper.get_extended_path())
