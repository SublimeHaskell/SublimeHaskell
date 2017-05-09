
import os

import sublime_plugin

import SublimeHaskell.autobuild as Autobuild
import SublimeHaskell.autocomplete as Autocomplete
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.types as Types


def plugin_loaded():
    '''All of the good stuff that happens when SublimeHaskell is loaded.
    '''
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
    '''Finalization actions when SublimeHaskell is unloaded.
    '''
    BackendManager.BackendManager().shutdown_backend()


class SublimeHaskellEventListener(sublime_plugin.EventListener):
    '''The plugin's primary SublimeText event listener, for consolidated actions related to file I/O (after saves, buffer
    modifications.)
    '''
    def __init__(self):
        super().__init__()
        self.autobuilder = Autobuild.SublimeHaskellAutobuilder()

    def on_post_save(self, view):
        filename = view.file_name()
        if filename:
            if Common.is_inspected_source(view):
                BackendManager.inspector().mark_file_dirty(filename)
                Autocomplete.update_completions_async(drop_all=True)
            if Common.is_haskell_source(view):
                Types.SourceHaskellTypeCache().remove(filename)
                self.autobuilder.on_post_save(view)

    def on_modified(self, view):
        if Common.is_haskell_source(view):
            self.autobuilder.on_modified(view)
            Types.SourceHaskellTypeCache().remove(view.file_name())
