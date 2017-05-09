
import os

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.backend_mgr as BackendManager


def plugin_loaded():
    cache_path = Common.sublime_haskell_cache_path()

    if not os.path.exists(cache_path):
        os.makedirs(cache_path)

    # Probably already loaded... doesn't hurt to reload.
    Settings.load_settings()

    # Register change detection:
    Settings.PLUGIN.add_change_callback('add_to_PATH', ProcHelper.ProcHelper.update_environment)
    Settings.PLUGIN.add_change_callback('add_standard_dirs', ProcHelper.ProcHelper.update_environment)

    # Now create the backend...
    backend = BackendManager.BackendManager()
    backend.initialize()


def plugin_unloaded():
    BackendManager.BackendManager().shutdown_backend()
