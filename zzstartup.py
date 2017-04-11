
import os

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.hdevtools as HDevTools
import SublimeHaskell.hsdev.agent as HsDevAgent


def plugin_loaded():
    cache_path = Common.sublime_haskell_cache_path()

    if not os.path.exists(cache_path):
        os.makedirs(cache_path)

    # Probably already loaded... doesn't hurt to reload.
    Settings.PLUGIN.load()

    # Register change detection:
    Settings.PLUGIN.add_change_callback('add_to_PATH', ProcHelper.ProcHelper.update_environment)
    Settings.PLUGIN.add_change_callback('add_standard_dirs', ProcHelper.ProcHelper.update_environment)

    # Deprecate?
    HDevTools.start_hdevtools()


def plugin_unloaded():
    # Does this work properly on exit?
    HDevTools.stop_hdevtools()
    # Shutdown hsdev
    if HsDevAgent.agent is not None:
        HsDevAgent.agent.stop_hsdev()
