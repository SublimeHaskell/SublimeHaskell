import errno
import fnmatch
import os
import re
import json
import sublime
import sublime_plugin
import subprocess
import threading
import time

# Maximum seconds to wait for window to appear
# This dirty hack is used in wait_for_window function
MAX_WAIT_FOR_WINDOW = 10

# Panel for SublimeHaskell errors
SUBLIME_ERROR_PANEL_NAME = 'haskell_sublime_load'

# Used to detect hs-source-dirs for project
CABAL_INSPECTOR_EXE_PATH = None

# Setting can't be get from not main threads
# So we using a trick:
# Once setting loaded from main thread, it also stored in sublime_haskell_settings dictionary
# and callback attached to update its value
# And then setting can be get from any thread with get_setting_async
# But setting must be loaded at least once from main thread
# Some settings are loaded only from secondary threads, so we loading them here for first time
def preload_settings():
    # Now we can use get_setting_async for 'add_to_PATH' safely
    get_setting('add_to_PATH')
    get_setting('use_cabal_dev')
    get_setting('cabal_dev_sandbox')
    get_setting('cabal_dev_sandbox_list')
    get_setting('enable_auto_build')
    get_setting('show_output_window')
    get_setting('enable_ghc_mod')
    get_setting('enable_hdevtools')
    get_setting('enable_hdocs')
    get_setting('snippet_replace')
    get_setting('ghc_opts')

# SublimeHaskell settings dictionary
# used to retrieve it async from any thread
sublime_haskell_settings = {}


def is_enabled_haskell_command(view = None, must_be_project=True, must_be_main=False, must_be_file = False):
    """Returns True if command for .hs can be invoked"""
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)

    if not window or not view:
        return False

    if must_be_file and not file_shown_in_view:
        return False

    syntax_file_for_view = view.settings().get('syntax').lower()
    if 'haskell' not in syntax_file_for_view:
        return False

    if not must_be_project:
        return True

    cabal_project_dir = get_cabal_project_dir_of_view(view)
    if not cabal_project_dir:
        return False
    return True


def get_haskell_command_window_view_file_project(view = None):
    """Returns window, view and file"""
    if view:
        return view.window(), view, view.file_name()

    window = sublime.active_window()
    view = None
    if window:
        view = window.active_view()
    file_name = None
    if view:
        file_name = view.file_name()
    return window, view, file_name


def decode_bytes(s):
    if s is None:
        return None
    return s.decode('utf-8')

def encode_bytes(s):
    if s is None:
        return None
    return s.encode('utf-8')

def call_and_wait(command, **popen_kwargs):
    return call_and_wait_with_input(command, None, **popen_kwargs)

def call_no_wait(command, **popen_kwargs):
    """Run the specified command with no block"""
    if subprocess.mswindows:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        popen_kwargs['startupinfo'] = startupinfo

    extended_env = dict(os.environ)
    PATH = os.getenv('PATH') or ""
    extended_env['PATH'] = ':'.join(get_setting_async('add_to_PATH', []) + [PATH])

    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        stdin=subprocess.PIPE,
        env=extended_env,
        **popen_kwargs)

def call_and_wait_with_input(command, input_string, **popen_kwargs):
    """Run the specified command, block until it completes, and return
    the exit code, stdout, and stderr.
    Extends os.environment['PATH'] with the 'add_to_PATH' setting.
    Additional parameters to Popen can be specified as keyword parameters."""
    if subprocess.mswindows:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        popen_kwargs['startupinfo'] = startupinfo

    # For the subprocess, extend the env PATH to include the 'add_to_PATH' setting.
    extended_env = dict(os.environ)
    PATH = os.getenv('PATH') or ""
    extended_env['PATH'] = ':'.join(get_setting_async('add_to_PATH', []) + [PATH])

    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        stdin=subprocess.PIPE,
        env=extended_env,
        **popen_kwargs)
    stdout, stderr = process.communicate(encode_bytes(input_string))
    exit_code = process.wait()
    return (exit_code, decode_bytes(stdout), decode_bytes(stderr))


def log(message):
    print(u'Sublime Haskell: {0}'.format(message))


def get_cabal_project_dir_and_name_of_view(view):
    """Return the path to the .cabal file project for the source file in the
    specified view. The view must show a saved file, the file must be Haskell
    source code, and the file must be under a directory containing a .cabal file.
    Otherwise, return None.
    """
    # Check that the view is showing a saved file:
    file_shown_in_view = view.file_name()
    if file_shown_in_view is None:
        return None, None
    # Check that the file is Haskell source code:
    syntax_file_for_view = view.settings().get('syntax').lower()
    if 'haskell' not in syntax_file_for_view:
        return None, None
    return get_cabal_project_dir_and_name_of_file(file_shown_in_view)


def get_cabal_project_dir_of_view(view):
    return get_cabal_project_dir_and_name_of_view(view)[0]


def get_cabal_project_dir_and_name_of_file(filename):
    """Return the path to the .cabal file and name of project for the specified file."""
    # Check that a .cabal file is present:
    directory_of_file = os.path.dirname(filename)
    cabal_file_path = find_file_in_parent_dir(directory_of_file, '*.cabal')
    if cabal_file_path is None:
        return None, None
    # Return the directory containing the .cabal file:
    project_path, cabal_file = os.path.split(cabal_file_path)
    project_name = os.path.splitext(cabal_file)[0]
    return project_path, project_name


def get_cabal_project_dir_of_file(filename):
    """Return the path to the .cabal file project for the specified file."""
    return get_cabal_project_dir_and_name_of_file(filename)[0]


def get_cabal_in_dir(cabal_dir):
    """Return .cabal file for cabal directory"""
    for entry in os.listdir(cabal_dir):
        if entry.endswith(".cabal"):
            project_name = os.path.splitext(entry)[0]
            return (project_name, os.path.join(cabal_dir, entry))
    return (None, None)


def find_file_in_parent_dir(subdirectory, filename_pattern):
    """Look for a file with the specified name in a parent directory of the
    specified directory. If found, return the file's full path. Otherwise,
    return None."""
    current_dir = subdirectory
    while True:
        # See if the current directory contains the desired file:
        for name in os.listdir(current_dir):
            full_path = os.path.join(current_dir, name)
            matches_pattern = fnmatch.fnmatch(name, filename_pattern)
            if matches_pattern and os.path.isfile(full_path):
                return full_path
        # Get the next directory up:
        last_dir = current_dir
        current_dir = os.path.dirname(current_dir)
        # Check to see if we have reached the root directory:
        if last_dir == current_dir:
            return None


def are_paths_equal(path, other_path):
    "Test whether filesystem paths are equal."
    path = os.path.abspath(path)
    other_path = os.path.abspath(other_path)
    return path == other_path


def current_cabal():
    """
    Returns current cabal-dev sandbox or 'cabal'
    """
    if get_setting_async('use_cabal_dev'):
        return get_setting_async('cabal_dev_sandbox')
    else:
        return 'cabal'

def current_sandbox():
    """
    Returns current cabal-def sandbox or None
    """
    if get_setting_async('use_cabal_dev'):
        return get_setting_async('cabal_dev_sandbox')
    else:
        return None

def cabal_name_by_sandbox(sandbox):
    if not sandbox:
        return current_cabal()
    return sandbox

def sandbox_by_cabal_name(cabal):
    if cabal == 'cabal':
        return None
    return cabal

def attach_sandbox(cmd, sandbox = None):
    """Attach sandbox arguments to command"""
    if not sandbox:
        sandbox = get_setting_async('cabal_dev_sandbox')
    if len(sandbox) > 0:
        return cmd + ['-s', sandbox]
    return cmd


def try_attach_sandbox(cmd, sandbox = None):
    """Attach sandbox if use_cabal_dev enabled"""
    if not get_setting_async('use_cabal_dev'):
        return cmd
    return attach_sandbox(cmd, sandbox)


def attach_cabal_sandbox(cmd, cabal = None):
    """
    Attach sandbox if cabal is sandbox path, attach nothing on 'cabal',
    and attach sandbox by settings on None
    """
    if not cabal:
        cabal = current_cabal()
    if cabal == 'cabal':
        return cmd
    return cmd + ['-s', cabal]


def get_settings():
    return sublime.load_settings("SublimeHaskell.sublime-settings")


def save_settings():
    sublime.save_settings("SublimeHaskell.sublime-settings")


def get_setting(key, default=None):
    "This should be used only from main thread"
    # Get setting
    result = get_settings().get(key, default)
    # Key was not retrieved, save its value and add callback to auto-update
    if key not in sublime_haskell_settings:
        sublime_haskell_settings[key] = result
        get_settings().add_on_change(key, lambda: update_setting(key))
    return result


def update_setting(key):
    "Updates setting as it was changed"
    sublime_haskell_settings[key] = get_setting(key)


def get_setting_async(key, default=None):
    """
    Get setting from any thread
    Note, that setting must be loaded before by get_setting from main thread
    """
    # Reload it in main thread for future calls of get_setting_async
    sublime.set_timeout(lambda: update_setting(key), 0)
    if key not in sublime_haskell_settings:
        # Load it in main thread, but for now all we can do is result default
        return default
    return sublime_haskell_settings[key]


def set_setting(key, value):
    """Set setting and update dictionary"""
    sublime_haskell_settings[key] = value
    get_settings().set(key, value)
    save_settings()

def set_setting_async(key, value):
    sublime.set_timeout(lambda: set_setting(key, value), 0)

def ghci_package_db(cabal = None):
    if cabal == 'cabal':
        return None
    dev = True if cabal else get_setting_async('use_cabal_dev')
    box = cabal if cabal else get_setting_async('cabal_dev_sandbox')
    if dev and box:
        package_conf = (filter(lambda x: re.match('packages-(.*)\.conf', x), os.listdir(box)) + [None])[0]
        if package_conf:
            return os.path.join(box, package_conf)
    return None

def ghci_append_package_db(cmd, cabal = None):
    package_conf = ghci_package_db(cabal)
    if package_conf:
        cmd.extend(['-package-db', package_conf])
    return cmd

def get_source_dir(filename):
    """
    Get root of hs-source-dirs for filename in project
    """
    if not filename:
        return os.path.expanduser('~')
        # return os.getcwd()

    (cabal_dir, project_name) = get_cabal_project_dir_and_name_of_file(filename)
    if not cabal_dir:
        return os.path.dirname(filename)

    _project_name, cabal_file = get_cabal_in_dir(cabal_dir)
    exit_code, out, err = call_and_wait([CABAL_INSPECTOR_EXE_PATH, cabal_file])

    if exit_code == 0:
        info = json.loads(out)

        dirs = ["."]

        if 'error' not in info:
            # collect all hs-source-dirs
            if info['library']:
                dirs.extend(info['library']['info']['source-dirs'])
            for i in info['executables']:
                dirs.extend(i['info']['source-dirs'])
            for t in info['tests']:
                dirs.extend(t['info']['source-dirs'])

        paths = [os.path.abspath(os.path.join(cabal_dir, d)) for d in dirs]
        paths.sort(key = lambda p: -len(p))

        for p in paths:
            if filename.startswith(p):
                return p

    return os.path.dirname(filename)

def get_cwd(filename = None):
    """
    Get cwd for filename: cabal project path, file path or os.getcwd()
    """
    cwd = (get_cabal_project_dir_of_file(filename) or os.path.dirname(filename)) if filename else os.getcwd()
    return cwd

def get_ghc_opts(filename = None, add_package_db = True, cabal = None):
    """
    Gets ghc_opts, used in several tools, as list with extra '-package-db' option and '-i' option if filename passed
    """
    ghc_opts = get_setting_async('ghc_opts')
    if not ghc_opts:
        ghc_opts = []
    if add_package_db:
        package_db = ghci_package_db(cabal = cabal)
        if package_db:
            ghc_opts.append('-package-db {0}'.format(package_db))

    if filename:
        ghc_opts.append('-i {0}'.format(get_source_dir(filename)))

    return ghc_opts

def get_ghc_opts_args(filename = None, add_package_db = True, cabal = None):
    """
    Same as ghc_opts, but uses '-g' option for each option
    """
    opts = get_ghc_opts(filename, add_package_db, cabal)
    args = []
    for opt in opts:
        args.extend(["-g", opt])
    return args

def call_ghcmod_and_wait(arg_list, filename=None, cabal = None):
    """
    Calls ghc-mod with the given arguments.
    Shows a sublime error message if ghc-mod is not available.
    """

    ghc_opts_args = get_ghc_opts_args(filename, add_package_db = False, cabal = cabal)

    try:
        command = attach_cabal_sandbox(['ghc-mod'] + arg_list + ghc_opts_args, cabal)

        # log('running ghc-mod: {0}'.format(command))

        # Set cwd to user directory
        # Otherwise ghc-mod will fail with 'cannot satisfy package...'
        # Seems, that user directory works well
        # Current source directory is set with -i argument in get_ghc_opts_args
        exit_code, out, err = call_and_wait(command, cwd=get_source_dir(None))

        if exit_code != 0:
            raise Exception("ghc-mod exited with status %d and stderr: %s" % (exit_code, err))

        return crlf2lf(out)

    except OSError as e:
        if e.errno == errno.ENOENT:
            output_error(sublime.active_window(),
                "SublimeHaskell: ghc-mod was not found!\n"
                + "It is used for LANGUAGE and import autocompletions and type inference.\n"
                + "Try adjusting the 'add_to_PATH' setting.\n"
                + "You can also turn this off using the 'enable_ghc_mod' setting.")

def wait_for_window_callback(on_appear, seconds_to_wait):
    window = sublime.active_window()
    if window:
        on_appear(window)
        return
    if seconds_to_wait == 0:
        return
    sublime.set_timeout(lambda: wait_for_window_callback(on_appear, seconds_to_wait - 1), 1000)


def wait_for_window(on_appear, seconds_to_wait=MAX_WAIT_FOR_WINDOW):
    """
    Wait for window to appear on startup
    It's dirty hack, but I have no idea how to make it better
    """
    sublime.set_timeout(lambda: wait_for_window_callback(on_appear, seconds_to_wait), 0)



class SublimeHaskellOutputText(sublime_plugin.TextCommand):
    """
    Helper command to output text to any view
    TODO: Is there any default command for this purpose?
    """
    def run(self, edit, text = None):
        if not text:
            return
        self.view.insert(edit, self.view.size(), text)



def output_error(window, text):
    "Write text to Sublime's output panel with important information about SublimeHaskell error during load"
    output_view = window.get_output_panel(SUBLIME_ERROR_PANEL_NAME)
    output_view.set_read_only(False)

    output_view.run_command('sublime_haskell_output_text', {
        'text': text})

    output_view.set_read_only(True)

    window.run_command('show_panel', {'panel': 'output.' + SUBLIME_ERROR_PANEL_NAME})

class SublimeHaskellError(RuntimeError):
    def __init__(self, what):
        self.reason = what

def sublime_status_message(msg):
    """
    Pure msg with 'SublimeHaskell' prefix and set_timeout
    """
    sublime.set_timeout(lambda: sublime.status_message(u'SublimeHaskell: {0}'.format(msg)), 0)

def show_status_message(msg, isok = None):
    """
    Show status message with check mark (isok = true), ballot x (isok = false) or ... (isok = None)
    """
    mark = u'...'
    if isok is not None:
        mark = u' \u2714' if isok else u' \u2718'
    sublime_status_message(u'{0}{1}'.format(msg, mark))

def with_status_message(msg, action):
    """
    Show status message for action with check mark or with ballot x
    Returns whether action exited properly
    """
    try:
        show_status_message(msg)
        action()
        show_status_message(msg, True)
        return True
    except SublimeHaskellError as e:
        show_status_message(msg, False)
        log(e.reason)
        return False

def crlf2lf(s):
    " CRLF -> LF "
    if not s:
        return ''
    return s.replace('\r\n', '\n')

class StatusMessage(threading.Thread):
    messages = {}
    # List of ((priority, time), StatusMessage)
    # At start, messages adds itself to list, at cancel - removes
    # First element of list is message with highest priority
    priorities_lock = threading.Lock()
    priorities = []

    def __init__(self, msg, timeout, priority):
        super(StatusMessage, self).__init__()
        self.interval = 0.5
        self.start_timeout = timeout
        self.timeout = timeout
        self.priority = priority
        self.msg = msg
        self.times = 0
        self.event = threading.Event()
        self.event.set()
        self.timer = None

    def run(self):
        self.add_to_priorities()
        try:
            self.update_message()
            while self.event.is_set():
                self.timer = threading.Timer(self.interval, self.update_message)
                self.timer.start()
                self.timer.join()
        finally:
            self.remove_from_priorities()

    def cancel(self):
        self.event.clear()
        if self.timer:
            self.timer.cancel()

    def update_message(self):
        dots = self.times % 4
        self.times += 1
        self.timeout -= self.interval

        if self.is_highest_priority():
            sublime_status_message(u'{0}{1}'.format(self.msg, '.' * dots))
    
        if self.timeout <= 0:
            self.cancel()

    def add_to_priorities(self):
        with StatusMessage.priorities_lock:
            StatusMessage.priorities.append(((self.priority, time.clock()), self))
            StatusMessage.priorities.sort(key = lambda x: (-x[0][0], x[0][1], x[1]))

    def remove_from_priorities(self):
        with StatusMessage.priorities_lock:
            StatusMessage.priorities = [(i, msg) for i, msg in StatusMessage.priorities if msg != self]

    def is_highest_priority(self):
        with StatusMessage.priorities_lock:
            if StatusMessage.priorities:
                return StatusMessage.priorities[0][1] == self
            else:
                return False

    def change_message(self, new_msg):
        # There's progress, don't timeout
        self.timeout = self.start_timeout
        self.msg = new_msg

def show_status_message_process(msg, isok = None, timeout = 300, priority = 0):
    """
    Same as show_status_message, but shows permanently until called with isok not None
    There can be only one message process in time, message with highest priority is shown
    For example, when building project, there must be only message about building
    """
    if isok is not None:
        if msg in StatusMessage.messages:
            StatusMessage.messages[msg].cancel()
            del StatusMessage.messages[msg]
        show_status_message(msg, isok)
    else:
        if msg in StatusMessage.messages:
            StatusMessage.messages[msg].cancel()

        StatusMessage.messages[msg] = StatusMessage(msg, timeout, priority)
        StatusMessage.messages[msg].start()

def is_haskell_source(view = None):
    return is_enabled_haskell_command(view, False)

class with_status_message(object):
    def __init__(self, msg, isok, show_message):
        self.msg = msg
        self.isok = isok
        self.show_message = show_message

    def __enter__(self):
        self.show_message(self.msg)
        return self

    def __exit__(self, type, value, traceback):
        if type:
            self.show_message(self.msg, False)
        else:
            self.show_message(self.msg, self.isok)

    def ok(self):
        self.isok = True

    def fail(self):
        self.isok = False

    def change_message(self, new_msg):
        if self.msg in StatusMessage.messages:
            StatusMessage.messages[self.msg].change_message(new_msg)

    def percentage_message(self, current, total = 100):
        self.change_message('{0} ({1}%)'.format(self.msg, int(current * 100 / total)))

def status_message(msg, isok = True):
    return with_status_message(msg, isok, show_status_message)

def status_message_process(msg, isok = True, timeout = 300, priority = 0):
    return with_status_message(msg, isok, lambda m, ok = None: show_status_message_process(m, ok, timeout, priority))

def sublime_haskell_package_path():
    """Get the path to where this package is installed"""
    return os.path.dirname(os.path.realpath(__file__))

def sublime_haskell_cache_path():
    """Get the path where compiled tools and caches are stored"""
    return os.path.join(sublime_haskell_package_path(), os.path.expandvars(get_setting('cache_path', '.')))

def plugin_loaded():
    global CABAL_INSPECTOR_EXE_PATH

    package_path = sublime_haskell_package_path()
    cache_path = sublime_haskell_cache_path()

    log("store compiled tools and caches to {0}".format(cache_path))
    if not os.path.exists(cache_path):
        os.makedirs(cache_path)

    CABAL_INSPECTOR_EXE_PATH = os.path.join(cache_path, 'CabalInspector')
    preload_settings()
    
if int(sublime.version()) < 3000:
    plugin_loaded()

class LockedObject(object):
    """
    Object with lock
    x = LockedObject(some_value)
    with x as v:
        v...
    """

    def __init__(self, obj, lock = None):
        self.object_lock = lock if lock else threading.Lock()
        self.object = obj

    def __enter__(self):
        self.object_lock.__enter__()
        return self.object

    def __exit__(self, type, value, traceback):
        self.object_lock.__exit__()
