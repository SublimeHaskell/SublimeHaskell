
import os.path
import threading

import sublime

import SublimeHaskell.internals.atomics as Atomics

SETTING_SUBHASK_PROJECT = 'subhask_project_name'
'''View-private setting that identifies the project to which a view belongs. This is the cabal file's name, without the
'.cabal'extension.'''

SETTING_SUBHASK_PROJDIR = 'subhask_project_dir'
'''View-private setting that identifies the project directory in which the project's cabal file can be found.'''

def make_config_property(attr):
    def locked_getter(self):
        with self.wlock:
            return getattr(self, '_' + attr)

    def locked_setter(self, value):
        with self.wlock:
            setattr(self, '_' + attr, value)

    return property(locked_getter, locked_setter)

def same_property_pref(prop):
    return (prop, prop)

KEY_COMPONENT_DEBUG_DEBUG = 'component_debug'
SUBHASK_PROJECT_KEY = 'SublimeHaskell'

class SettingsContainer(object):
    '''Container object for default and user preference settings.
    '''

    config_properties = dict([
        same_property_pref('add_default_completions'),
        same_property_pref('add_standard_dirs'),
        ('add_to_path', 'add_to_PATH'),
        same_property_pref('add_word_completions'),
        same_property_pref('auto_build_mode'),
        same_property_pref('auto_complete_imports'),
        same_property_pref('auto_complete_language_pragmas'),
        same_property_pref('auto_completion_popup'),
        same_property_pref('backends'),
        same_property_pref(KEY_COMPONENT_DEBUG_DEBUG),
        same_property_pref('enable_auto_build'),
        same_property_pref('enable_auto_check'),
        same_property_pref('enable_auto_lint'),
        same_property_pref('enable_hdocs'),
        same_property_pref('ghc_opts'),
        same_property_pref('ghci_opts'),
        same_property_pref('haskell_build_tool'),
        same_property_pref('hindent_options'),
        same_property_pref('hsdev_log_config'),
        same_property_pref('hsdev_log_level'),
        same_property_pref('inspect_modules'),
        same_property_pref('lint_check_fly'),
        same_property_pref('lint_check_fly_idle'),
        same_property_pref('log'),
        same_property_pref('prettify_on_save'),
        same_property_pref('prettify_executable'),
        same_property_pref('show_error_window'),
        same_property_pref('show_only'),
        same_property_pref('show_output_window'),
        same_property_pref('stylish_options'),
        same_property_pref('unicode_symbol_info'),
        same_property_pref('use_improved_syntax')
    ])


    def __init__(self):
        # Property value holders:
        self._add_default_completions = False
        self._add_standard_dirs = True
        self._add_to_path = []
        self._add_word_completions = False
        self._auto_build_mode = 'normal-then-warnings'
        self._auto_complete_imports = True
        self._auto_complete_language_pragmas = True
        self._auto_completion_popup = False
        self._backends = {}
        self._component_debug = []
        self._enable_auto_build = False
        self._enable_auto_check = True
        self._enable_auto_lint = True
        self._enable_hdocs = False
        self._ghc_opts = []
        self._ghci_opts = []
        self._haskell_build_tool = 'stack'
        self._hindent_options = []
        self._hsdev_log_config = 'use silent'
        self._hsdev_log_level = 'warning'
        self._inspect_modules = True
        self._lint_check_fly = False
        self._lint_check_fly_idle = 5
        self._log = 1
        self._prettify_on_save = False
        self._prettify_executable = 'stylish-haskell'
        self._show_error_window = True
        self._show_only = {'errors': True,
                           'warnings': True,
                           'hints': True}
        self._show_output_window = None
        self._stylish_options = []
        self._unicode_symbol_info = True
        self._use_improved_syntax = True

        # Additional change callbacks to propagate:
        self.changes = Atomics.AtomicDuck()
        # Write-access lock
        self.wlock = threading.RLock()


    ## And the properties themselves... Note that the property's getter/setter functions are actually
    ## wrappers created by make_config_property, which ensures thread-safe access to the underlying
    ## value holder.

    add_default_completions = make_config_property('add_default_completions')
    add_standard_dirs = make_config_property('add_standard_dirs')
    add_to_path = make_config_property('add_to_path')
    add_word_completions = make_config_property('add_word_completions')
    auto_build_mode = make_config_property('auto_build_mode')
    auto_complete_imports = make_config_property('auto_complete_imports'),
    auto_complete_language_pragmas = make_config_property('auto_complete_language_pragmas')
    auto_completion_popup = make_config_property('auto_completion_popup')
    backends = make_config_property('backends')
    component_debug = make_config_property('component_debug')
    enable_auto_build = make_config_property('enable_auto_build')
    enable_auto_check = make_config_property('enable_auto_check')
    enable_auto_lint = make_config_property('enable_auto_lint')
    enable_hdocs = make_config_property('enable_hdocs')
    ghc_opts = make_config_property('ghc_opts')
    ghci_opts = make_config_property('ghci_opts')
    haskell_build_tool = make_config_property('haskell_build_tool')
    hindent_options = make_config_property('hindent_options')
    hsdev_log_config = make_config_property('hsdev_log_config')
    hsdev_log_level = make_config_property('hsdev_log_level')
    inspect_modules = make_config_property('inspect_modules')
    lint_check_fly = make_config_property('lint_check_fly')
    lint_check_fly_idle = make_config_property('lint_check_fly_idle')

    # Of course, we have a couple of properties where we'd like to validate the new value.

    @property
    def log(self):
        with self.wlock:
            return self._log

    @log.setter
    def log(self, newval):
        with self.wlock:
            if newval >= 0 and newval <= 5:
                self._log = newval
            else:
                msg = ['Invalid log level: {0} (0 <= log <= 5), keeping current level at {1}']
                sublime.message_dialog('\n'.join(msg).format(newval, self._log))

    prettify_on_save = make_config_property('prettify_on_save')

    @property
    def prettify_executable(self):
        with self.wlock:
            return self._prettify_executable
    @prettify_executable.setter
    def prettify_executable(self, newval):
        with self.wlock:
            if newval:
                if os.path.exists(newval) or newval in ['stylish-haskell', 'hindent']:
                    self._prettify_executable = newval
                else:
                    msg = ['\'{0}\' is not a recognized Haskell indenter/prettifier. Recognized prettifiers are:',
                           '',
                           'stylish-haskell',
                           'hindent',
                           '',
                           'Please check your \'prettify_executable\' preference.']
                    sublime.message_dialog('\n'.join(msg).format(newval))
            elif self.prettify_on_save:
                msg = ['The \'prettify_executable\' setting is missing from the plugin\'s  settings.',
                       'This affects prettify-on-save functionality, which is now set to false.']
                self.prettify_on_save = False
                sublime.message_dialog('\n'.join(msg))


    show_error_window = make_config_property('show_error_window')

    @property
    def show_only(self):
        return self._show_only

    @show_only.setter
    def show_only(self, newval):
        if isinstance(newval, dict):
            self._show_only = newval
        else:
            msg = ['The \'show_only\' setting should be a dictionary, e.g.:',
                   '',
                   '  show_only: {',
                   '    "errors": true,'
                   '    "warnings": true,'
                   '    "hints": false',
                   '  }',
                   '',
                   'Please refer to SublimeHaskell\'s default settings for documentation.']
            sublime.message_dialog('\n'.join(msg))

    show_output_window = make_config_property('show_output_window')
    stylish_options = make_config_property('stylish_options')
    unicode_symbol_info = make_config_property('unicode_symbol_info')
    use_improved_syntax = make_config_property('use_improved_syntax')


    def load(self):
        settings = get_settings()
        self.check_preferences(settings)
        with self.wlock:
            for (attr, preference) in self.config_properties.items():
                value = settings.get(preference)
                if value:
                    ## Uncomment to debug. Do NOT use logging because it causes a circular dependency.
                    ## print('Settings.load: {0} = {1}'.format(attr, value))
                    setattr(self, attr, value)

                install_updater(settings, self, attr)
        self.changes = Atomics.AtomicDuck()

    def check_preferences(self, settings):
        ## New backend upgrade warning:
        old_stuff = []
        for old_setting in ['enable_hsdev', 'enable_ghc_mod', 'enable_hdevtools', 'hdevtools_socket',
                            'hsdev_host', 'hsdev_local_process', 'hsdev_port']:
            if settings.get(old_setting) is not None:
                old_stuff.append(old_setting)
        if old_stuff:
            msg = ['Old SublimeHaskell backend settings found:',
                   '']
            msg = msg + old_stuff
            msg = msg + ['',
                         'You are using the default SublimeHaskell settings'
                         'for the \'backend\' preference.',
                         '',
                         'Please look at the default settings and customize/migrate',
                         'them as needed in your user settings.',
                         '',
                         '(Preferences > Package Settings > SublimeHaskell)']
            sublime.message_dialog('\n'.join(msg))

        if settings.get('add_to_path'):
            msg = ['\'add_to_path\' setting detected. You probably meant \'add_to_PATH\'.']
            sublime.message_dialog('\n'.join(msg))

        if settings.get('auto_run_tests'):
            msg = ['\'auto_run_tests\' setting deprecated. See the new options in the ',
                   '\'auto_build_mode\' preference']
            sublime.message_dialog(''.join(msg))

        if settings.get('inhibit_completions'):
            msg = ['The \'inhibit_completions\' setting has been replaced by '
                   '\'add_word_completions\' and \'add_default_completions\'',
                   '',
                   'Please customize your settings with these two flags, '
                   'delete the \'inhibit_completions\' setting.']
            sublime.message_dialog('\n'.join(msg))


    def update_setting(self, attr):
        settings = get_settings()
        preference = self.config_properties[attr]
        oldval = getattr(self, attr)
        newval = settings.get(preference, oldval)
        if oldval != newval:
            # Only acquire the lock when we really need it.
            ## print('update: {0}/{3} -> old {1} new {2}'.format(attr, oldval, newval, preference))
            with self.wlock:
                if attr == KEY_COMPONENT_DEBUG_DEBUG:
                    COMPONENT_DEBUG.load(newval)
                else:
                    setattr(self, attr, newval)
                with self.changes as changes_:
                    for change_fn in changes_.get(attr, []):
                        change_fn(attr, newval)

    def add_change_callback(self, key, change_fn):
        with self.wlock:
            with self.changes as changes_:
                if key not in changes_:
                    changes_[key] = []

                changes_[key].append(change_fn)


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
    subhask_data = (view.window().project_data() or {}).get(SUBHASK_PROJECT_KEY, {})
    return subhask_data.get(key, default)

def set_project_setting(view, key, value):
    project_data = view.window().project_data()
    if SUBHASK_PROJECT_KEY not in project_data:
        project_data[SUBHASK_PROJECT_KEY] = {}

    project_data[SUBHASK_PROJECT_KEY][key] = value

    return view.window().set_project_data(project_data)


class ComponentDebug(object):
    '''Convenience container for backend debugging settings.
    '''
    def __init__(self):
        self.all_messages = False
        self.callbacks = False
        self.completions = False
        self.event_viewer = False
        self.fly_mode = False
        self.inspection = False
        self.recv_messages = False
        self.send_messages = False
        self.socket_pool = False

    def load(self, backend_settings):
        self.all_messages = 'all_messages' in backend_settings
        self.callbacks = 'callbacks' in backend_settings
        self.completions = 'completions' in backend_settings
        self.event_viewer = 'event_viewer' in backend_settings
        self.fly_mode = 'fly_mode' in backend_settings
        self.inspection = 'inspection' in backend_settings
        self.recv_messages = 'recv_messages' in backend_settings
        self.send_messages = 'send_messages' in backend_settings
        self.socket_pool = 'socket_pool' in backend_settings


PLUGIN = SettingsContainer()
COMPONENT_DEBUG = ComponentDebug()

def load_settings():
    '''Instantiate the SettingsContainer module instance, which happens as part of the module loading in the
    main thread. Across reloads, though, try to keep the update triggers.
    '''
    global PLUGIN
    global COMPONENT_DEBUG

    _changes = None
    if 'PLUGIN' in globals():
        _plugin = globals()['PLUGIN']
        if _plugin is not None:
            _changes = _plugin.changes

    PLUGIN = SettingsContainer()
    COMPONENT_DEBUG = ComponentDebug()

    PLUGIN.load()
    COMPONENT_DEBUG.load(PLUGIN.component_debug or [])

    if _changes is not None:
        PLUGIN.changes = _changes
