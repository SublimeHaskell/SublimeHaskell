# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell settings management
#
# This is a hack so that Sublime's Settings work outside of the main thread.
# You cannot use the Settings class' methods outside of the main thread, so
# SublimeHaskell has to keep its own copy of pertinent settings in its own
# (lock controlled) dictionary.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import sublime

if int(sublime.version()) < 3000:
    # from internals.locked_object import LockedObject
    pass
else:
    import SublimeHaskell.internals.locked_object as LockedObject

# SublimeHaskell settings dictionary used to retrieve a settnig asynchronously from any thread
sublime_haskell_settings = LockedObject.LockedObject({})

# Callbacks for updated settings/change detection
sublime_settings_changes = LockedObject.LockedObject({})


def get_settings():
    return sublime.load_settings('SublimeHaskell.sublime-settings')


def save_settings():
    sublime.save_settings("SublimeHaskell.sublime-settings")


def get_setting(key, default=None):
    "This should be used only from main thread"
    result = get_settings().get(key, default)
    with sublime_haskell_settings as settings:
        settings[key] = result
    return result


def update_setting(key):
    get_setting(key)


def on_changed_setting(key):
    "Updates setting as it was changed"
    with sublime_haskell_settings as settings:
        old_val = settings.get(key)
        val = get_setting(key)
        # print("on_changed_settings: key {0} -> val != old_val {1}".format(key, old_val != val))
        if old_val is not None and old_val != val:
            with sublime_settings_changes as changes:
                if key in changes:
                    for change_fn in changes[key]:
                        change_fn(key, val)


def get_setting_async(key, default=None):
    """
    Get setting from any thread
    Note, that setting must be loaded before by get_setting from main thread
    """
    # Reload it in main thread for future calls of get_setting_async
    sublime.set_timeout(lambda: update_setting(key), 0)
    with sublime_haskell_settings as settings:
        if key not in settings:
            # Load it in main thread, but for now all we can do is result default
            return default
        res = settings[key]
        if res is None:
            return default
        return res


def set_setting(key, value):
    """Set setting and update dictionary"""
    with sublime_haskell_settings as settings:
        settings[key] = value
    get_settings().set(key, value)
    save_settings()


def set_setting_async(key, value):
    sublime.set_timeout(lambda: set_setting(key, value), 0)


def subscribe_setting(key, change_fn):
    with sublime_settings_changes as changes:
        if key not in changes:
            changes[key] = []
        changes[key].append(change_fn)
