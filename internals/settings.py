# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# SublimeHaskell settings management
#
# This is a hack so that Sublime's Settings work outside of the main thread.
# You cannot use the Settings class' methods outside of the main thread, so
# SublimeHaskell has to keep its own copy of pertinent settings in its own
# (lock controlled) dictionary.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import threading

import sublime

import SublimeHaskell.internals.locked_object as LockedObject

def access_sync(lock_name):
    """Decorate a function that requires lock synchronization: acquire the lock named `lock_name` (a member of an object)
    and execute a function. This ensures that readers and writers don't collide with each other."""
    def decorator(method):
        def synced_method(self, *args, **kwargs):
            # Because we use this on an object's __getattribute__ method, we have to call object.__getattribute__
            # directly to avoid infinite recursion.
            lock = object.__getattribute__(self, lock_name)
            with lock:
                return method(self, *args, **kwargs)
        return synced_method
    return decorator


class SetttingsContainer(object):
    """Container object for default and user preference settings."""

    # Map instance settings keys to attributes and default values.
    # Key: Setting name
    # Value: (Instance attribute, default value)
    #
    # Note: Must keep this consistent with the instance attributes in __init__()
    # and with the SublimeHaskell.sublime-settings file's contents.
    attr_dict = {
        'add_standard_dirs': ('add_standard_dirs', True),
        'add_to_PATH': ('add_to_path', []),
        'auto_build_mode': ('auto_build_mode', 'normal-then-warnings'),
        'auto_complete_imports': ('auto_complete_imports', True),
        'auto_complete_language_pragmas': ('auto_complete_language_pragmas', True),
        'auto_completion_popup': ('auto_completion_popup', False),
        'auto_run_tests': ('auto_run_tests', True),
        'enable_auto_build': ('enable_auto_build', False),
        'enable_auto_check': ('enable_auto_check', True),
        'enable_auto_lint': ('enable_auto_lint', True),
        'enable_ghc_mod': ('enable_ghc_mod', True),
        'enable_hdevtools': ('enable_hdevtools', True),
        'enable_hdocs': ('enable_hdocs', False),
        'enable_hsdev': ('enable_hsdev', True),
        'ghc_opts': ('ghc_opts', []),
        'ghci_opts': ('ghci_opts', []),
        'haskell_build_tool': ('haskell_build_tool', 'stack'),
        'hdevtools_socket': ('hdevtools_socket', ''),
        'hsdev_host': ('hsdev_host', 'localhost'),
        'hsdev_local_process': ('hsdev_local_process', True),
        'hsdev_log_config': ('hsdev_log_config', 'use silent'),
        'hsdev_log_level': ('hsdev_log_level', 'warning'),
        'hsdev_port': ('hsdev_port', 4567),
        'inhibit_completions': ('inhibit_completions', False),
        'inspect_modules': ('inspect_modules', True),
        'lint_check_fly': ('lint_check_fly', False),
        'lint_check_fly_idle': ('lint_check_fly_idle', 5),
        'log': ('log', 1),
        'show_error_window': ('show_error_window', True),
        'show_output_window': ('show_output_window', True),
        'unicode_symbol_info': ('unicode_symbol_info', True),
        'use_improved_syntax': ('use_improved_syntax', True)
    }

    def __init__(self):
        # Instantiate the attributes (rationale: style and pylint error checking)
        self.add_standard_dirs = None
        self.add_to_path = []
        self.auto_build_mode = None
        self.auto_complete_imports = None
        self.auto_complete_language_pragmas = None
        self.auto_completion_popup = None
        self.auto_run_tests = None
        self.enable_auto_build = None
        self.enable_auto_check = None
        self.enable_auto_lint = None
        self.enable_ghc_mod = None
        self.enable_hdevtools = None
        self.enable_hdocs = None
        self.enable_hsdev = None
        self.ghc_opts = None
        self.ghci_opts = None
        self.haskell_build_tool = None
        self.hdevtools_socket = None
        self.hsdev_host = None
        self.hsdev_local_process = None
        self.hsdev_log_config = None
        self.hsdev_log_level = None
        self.hsdev_port = None
        self.inhibit_completions = None
        self.inspect_modules = None
        self.lint_check_fly = None
        self.lint_check_fly_idle = None
        self.log = None
        self.show_error_window = None
        self.show_output_window = None
        self.unicode_symbol_info = None
        self.use_improved_syntax = None

        # Set attributes to their respective default values:
        for (attr, default) in SetttingsContainer.attr_dict.values():
            setattr(self, attr, default)

        # Additional change callbacks to propagate:
        self.changes = LockedObject.LockedObject({})
        # Write-access lock
        self.wlock = threading.RLock()

    @access_sync('wlock')
    def __getattribute__(self, attr):
        return object.__getattribute__(self, attr)

    @access_sync('wlock')
    def load(self):
        settings = get_settings()
        for (key, (attr, default)) in SetttingsContainer.attr_dict.items():
            value = settings.get(key, default)
            ## Uncomment to debug. Do NOT use logging because it causes a circular dependency.
            ## print('Settings.load: {0} = {1}'.format(attr, value))
            setattr(self, attr, value)
            install_updater(settings, self, key)
        self.changes = LockedObject.LockedObject({})

    def update_setting(self, key):
        settings = get_settings()
        (attr, default) = SetttingsContainer.attr_dict[key]
        oldval = getattr(self, attr)
        newval = settings.get(key, default)
        if oldval != newval:
            # Only acquire the lock when we really need it.
            with self.wlock:
                setattr(self, attr, newval)
                with self.changes as changes:
                    for change_fn in changes.get(key, []):
                        change_fn(key, newval)

    @access_sync('wlock')
    def add_change_callback(self, key, change_fn):
        with self.changes as changes:
            if key not in changes:
                changes[key] = []

            changes[key].append(change_fn)


def install_updater(settings, setting_obj, key):
    def inner_update():
        setting_obj.update_setting(key)

    settings.clear_on_change(key)
    settings.add_on_change(key, inner_update)


def get_settings():
    return sublime.load_settings('SublimeHaskell.sublime-settings')


def save_settings():
    sublime.save_settings("SublimeHaskell.sublime-settings")

def get_project_setting(view, key, default=None):
    return view.window().project_data().get(key, default)

def set_project_setting(view, key, value):
    view.window().project_data().set(key, value)

# Preserve settings across plugin reloads:
if 'PLUGIN' not in globals():
    PLUGIN = SetttingsContainer()
else:
    PLUGIN = globals()['PLUGIN']
