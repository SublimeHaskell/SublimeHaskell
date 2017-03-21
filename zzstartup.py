
import os

import sublime

if int(sublime.version()) < 3000:
    import sublime_haskell_common as Common
    import internals.settings as Settings
    import internals.proc_helper as ProcHelper
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.settings as Settings
    import SublimeHaskell.internals.proc_helper as ProcHelper

# Can't retrieve settings from child threads, only from the main thread.
#
# So we use the following hack: Initially load settings from the main thread and
# store them in the sublime_haskell_settings dictionary and callback attached to
# update its value. And then setting can be retrieved from any thread with
# get_setting_async. The setting must be loaded at least once from main thread.
#
# Some settings are loaded only from secondary threads, so we load them here for
# first time.
def preload_settings():
    def change_func(key):
        return lambda: Settings.on_changed_setting(str(key))

    for key in ['add_standard_dirs'
                , 'add_to_PATH'
                , 'enable_auto_build'
                , 'haskell_build_tool'
                , 'show_error_window'
                , 'show_output_window'
                , 'enable_ghc_mod'
                , 'enable_hdevtools'
                , 'enable_hdocs'
                , 'enable_hsdev'
                , 'hsdev_log_config'
                , 'inspect_modules'
                , 'snippet_replace'
                , 'lint_check_fly'
                , 'lint_check_fly_idle'
                , 'ghc_opts'
                , 'log'
                , 'use_improved_syntax'
               ]:
        with Settings.sublime_haskell_settings as settings:
            subl_settings = Settings.get_settings()
            settings[key] = subl_settings.get(key)

            subl_settings.add_on_change(key, change_func(key))
            with Settings.sublime_settings_changes as changes:
                changes[key] = []

    # Register change detection:
    with Settings.sublime_settings_changes as changes:
        changes['add_to_PATH'].append(ProcHelper.ProcHelper.update_environment)
        changes['add_standard_dirs'].append(ProcHelper.ProcHelper.update_environment)


def plugin_loaded():
    cache_path = Common.sublime_haskell_cache_path()

    if Common.status_message_manager is None:
        Common.status_message_manager = Common.StatusMessagesManager()
        Common.status_message_manager.start()

    if not os.path.exists(cache_path):
        os.makedirs(cache_path)

    preload_settings()

if int(sublime.version()) < 3000:
    plugin_loaded()
