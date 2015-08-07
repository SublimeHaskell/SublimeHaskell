# -*- coding: UTF-8 -*-

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
from sys import version

PyV3 = version[0] == "3"

# Maximum seconds to wait for window to appear
# This dirty hack is used in wait_for_window function
MAX_WAIT_FOR_WINDOW = 10

# Panel for SublimeHaskell errors
SUBLIME_ERROR_PANEL_NAME = 'haskell_sublime_load'

# Used to detect hs-source-dirs for project
CABAL_INSPECTOR_EXE_PATH = None

def python3():
    return PyV3

# unicode function
def to_unicode(s):
    return s if PyV3 else unicode(s)

# Object with lock attacjed
class LockedObject(object):
    """
    Object with lock
    x = LockedObject(some_value)
    with x as v:
        v...
    """

    def __init__(self, obj, lock = None):
        self.object_lock = lock if lock else threading.RLock()
        self.object = obj

    def __enter__(self):
        self.object_lock.__enter__()
        return self.object

    def __exit__(self, type, value, traceback):
        self.object_lock.__exit__(type, value, traceback)

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
    get_setting('use_cabal_sandbox')
    get_setting('cabal_sandbox')
    get_setting('cabal_sandbox_list')
    get_setting('enable_auto_build')
    get_setting('show_output_window')
    get_setting('enable_ghc_mod')
    get_setting('enable_hdevtools')
    get_setting('enable_hdocs')
    get_setting('enable_hsdev')
    get_setting('hsdev_log_config')
    get_setting('inspect_modules')
    get_setting('snippet_replace')
    get_setting('lint_check_fly')
    get_setting('ghc_opts')
    get_setting('log')

# SublimeHaskell settings dictionary
# used to retrieve it async from any thread
sublime_haskell_settings = LockedObject({})
# Callbacks on change settings
sublime_settings_changes = LockedObject({})

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

def call_and_wait(command, split_lines = False, **popen_kwargs):
    return call_and_wait_with_input(command, '', split_lines, **popen_kwargs)

def call_no_wait(command, **popen_kwargs):
    """Run the specified command with no block"""
    if subprocess.mswindows:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        popen_kwargs['startupinfo'] = startupinfo

    extended_env = get_extended_env()

    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        stdin=subprocess.PIPE,
        env=extended_env,
        **popen_kwargs)

# Get extended environment from settings for Popen
def get_extended_env():
    ext_env = dict(os.environ)
    PATH = os.getenv('PATH') or ""
    add_to_PATH = get_setting_async('add_to_PATH', [])
    if not PyV3:
        # convert unicode strings to strings (for Python < 3) as env can contain only strings
        add_to_PATH = map(str, add_to_PATH)
    ext_env['PATH'] = os.pathsep.join(add_to_PATH + [PATH])
    return ext_env

def call_and_wait_tool(command, tool_name, input = '', on_result = None, filename = None, on_line = None, check_enabled = True, **popen_kwargs):
    tool_enabled = 'enable_{0}'.format(tool_name)

    if check_enabled and get_setting_async(tool_enabled) != True:
        return None
    extended_env = get_extended_env()

    source_dir = get_source_dir(filename)

    def mk_result(s):
        return on_result(s) if on_result else s

    try:
        if on_line:
            for l in call_and_wait_with_input(command, input, split_lines = True, cwd = source_dir, **popen_kwargs):
                on_line(mk_result(crlf2lf(decode_bytes(l))))
            return None
        else:
            exit_code, out, err = call_and_wait_with_input(command, input, cwd = source_dir, **popen_kwargs)
            out = crlf2lf(out)

            if exit_code != 0:
                raise Exception('{0} exited with status {1} and stderr: {2}'.format(tool_name, exit_code, err))

            return mk_result(out)

    except OSError as e:
        if e.errno == errno.ENOENT:
            output_error_async(sublime.active_window(), "SublimeHaskell: {0} was not found!\n'{1}' is set to False".format(tool_name, tool_enabled))
            set_setting_async(tool_enabled, False)
        else:
            log('{0} fails with {1}, command: {2}'.format(tool_name, e, command), log_error)

        return None

    except Exception as e:
        log('{0} fails with {1}, command: {2}'.format(tool_name, e, command), log_error)

    return None

def call_and_wait_with_input(command, input_string, split_lines = False, **popen_kwargs):
    """Run the specified command, block until it completes, and return
    the exit code, stdout, and stderr.
    Extends os.environment['PATH'] with the 'add_to_PATH' setting.
    Additional parameters to Popen can be specified as keyword parameters."""
    if subprocess.mswindows:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        popen_kwargs['startupinfo'] = startupinfo

    # For the subprocess, extend the env PATH to include the 'add_to_PATH' setting.
    extended_env = get_extended_env()

    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        stdin=subprocess.PIPE,
        env=extended_env,
        **popen_kwargs)

    if split_lines:
        process.stdin.write(encode_bytes(input_string))
        process.stdin.close()
        return process.stdout
    else:
        stdout, stderr = process.communicate(encode_bytes(input_string))
        exit_code = process.wait()
        return (exit_code, crlf2lf(decode_bytes(stdout)), crlf2lf(decode_bytes(stderr)))

log_error = 1
log_warning = 2
log_info = 3
log_debug = 4
log_trace = 5

def log(message, level = log_info):
    log_level = get_setting_async('log', log_info)
    if log_level >= level:
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

def is_cabal(cabal):
    if cabal == 'cabal':
        return True
    if cabal is None:
        return None
    return False

def current_is_cabal():
    return is_cabal(current_cabal())

def as_sandboxes(cabal):
    if cabal == 'cabal':
        return []
    if cabal is None:
        return None
    return [cabal]

def current_cabal():
    """
    Returns current cabal-dev sandbox or 'cabal'
    """
    if get_setting_async('use_cabal_sandbox'):
        return get_setting_async('cabal_sandbox')
    else:
        return 'cabal'

def current_sandbox():
    """
    Returns current cabal-def sandbox or None
    """
    if get_setting_async('use_cabal_sandbox'):
        return get_setting_async('cabal_sandbox')
    else:
        return None

def current_sandboxes():
    return as_sandboxes(current_sandbox())

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
        sandbox = get_setting_async('cabal_sandbox')
    if len(sandbox) > 0:
        return cmd + ['-s', sandbox]
    return cmd


def try_attach_sandbox(cmd, sandbox = None):
    """Attach sandbox if use_cabal_sandbox enabled"""
    if not get_setting_async('use_cabal_sandbox'):
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
    with sublime_haskell_settings as settings:
        if key not in settings:
            get_settings().add_on_change(key, lambda: on_changed_setting(key))
        settings[key] = result
    return result

def update_setting(key):
    get_setting(key)

def on_changed_setting(key):
    "Updates setting as it was changed"
    with sublime_haskell_settings as settings:
        old_val = settings.get(key)
    val = get_setting(key)
    if (old_val is not None) and (old_val != val):
        with sublime_settings_changes as changes:
            if key in changes:
                for fn in changes[key]:
                    fn(key, val)


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
        return settings[key]


def set_setting(key, value):
    """Set setting and update dictionary"""
    with sublime_haskell_settings as settings:
        settings[key] = value
    get_settings().set(key, value)
    save_settings()

def set_setting_async(key, value):
    sublime.set_timeout(lambda: set_setting(key, value), 0)


def subscribe_setting(key, fn):
    with sublime_settings_changes as changes:
        if key not in changes:
            changes[key] = []
        changes[key].append(fn)


def ghci_package_db(cabal = None):
    if cabal == 'cabal':
        return None
    dev = True if cabal else get_setting_async('use_cabal_sandbox')
    box = cabal if cabal else get_setting_async('cabal_sandbox')
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
    exit_code, out, err = call_and_wait(['hsinspect', cabal_file])

    if exit_code == 0:
        info = json.loads(out)

        dirs = ["."]

        if 'error' not in info and 'description' in info:
            # collect all hs-source-dirs
            descr = info['description']
            if descr['library']:
                dirs.extend(descr['library']['info']['source-dirs'])
            for i in descr['executables']:
                dirs.extend(i['info']['source-dirs'])
            for t in descr['tests']:
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
        #
        # When cabal project is available current directory is set to the project root
        # to avoid troubles with possible template haskell openFile calls
        ghc_mod_current_dir = get_source_dir(filename)
        if filename:
            cabal_project_dir = get_cabal_project_dir_of_file(filename)
            if cabal_project_dir:
                ghc_mod_current_dir = cabal_project_dir
        exit_code, out, err = call_and_wait(command, cwd=ghc_mod_current_dir)

        if exit_code != 0:
            raise Exception("%s exited with status %d and stderr: %s" % (' '.join(command), exit_code, err))

        return crlf2lf(out)

    except OSError as e:
        if e.errno == errno.ENOENT:
            output_error_async(sublime.active_window(),
                "SublimeHaskell: ghc-mod was not found!\n"
                + "It is used for LANGUAGE and import autocompletions and type inference.\n"
                + "Try adjusting the 'add_to_PATH' setting.\n"
                + "You can also turn this off using the 'enable_ghc_mod' setting.")
        # Re-raise so that calling code doesn't try to work on the `None` return value
        raise e

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
    def run(self, edit, text = None, clear = None):
        if not text:
            return
        self.view.set_read_only(False)
        if clear:
            self.view.erase(edit, sublime.Region(0, self.view.size()))
        self.view.insert(edit, self.view.size(), text)
        self.view.set_read_only(True)

def output_text(view, text = None, clear = False):
    view.run_command('sublime_haskell_output_text', { 'text': (text or ''), 'clear': 'yes' if clear else '' })

def output_panel(window, text = '', panel_name = 'sublime_haskell_output_panel', syntax = None, show_panel = True):
    if not window:
        return None
    output_view = window.get_output_panel(panel_name)
    if syntax is not None:
        output_view.set_syntax_file('Packages/SublimeHaskell/Syntaxes/{0}.tmLanguage'.format(syntax))
    output_text(output_view, text, clear = True)
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0, 0))
    if show_panel:
        window.run_command('show_panel', { 'panel': ('output.' + panel_name) })
    return output_view

def hide_panel(window, panel_name = 'sublime_haskell_output_panel'):
    window.run_command('hide_panel', { 'panel': ('output.' + panel_name) })

def show_panel(window, panel_name = 'sublime_haskell_output_panel'):
    window.run_command('show_panel', { 'panel': ('output.' + panel_name) })

def output_error(window, text):
    output_panel(window, text, panel_name = SUBLIME_ERROR_PANEL_NAME)

def output_error_async(window, text):
    sublime.set_timeout(lambda: output_error(window, text), 0)

class SublimeHaskellError(RuntimeError):
    def __init__(self, what):
        self.reason = what

def sublime_status_message(msg):
    """
    Pure msg with 'SublimeHaskell' prefix and set_timeout
    """
    sublime.set_timeout(lambda: sublime.status_message(u'SublimeHaskell: {0}'.format(msg)), 0)

# def show_status_message(msg, is_ok = None):
#     """
#     Show status message with check mark (is_ok = true), ballot x (is_ok = false) or ... (is_ok = None)
#     """
#     mark = u'...'
#     if is_ok is not None:
#         mark = u' \u2714' if is_ok else u' \u2718'
#     sublime_status_message(u'{0}{1}'.format(msg, mark))

def crlf2lf(s):
    " CRLF -> LF "
    if s is None:
        return None
    if not s:
        return ''
    return s.replace('\r\n', '\n')

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
        self.object_lock.__exit__(value, type, traceback)

class StatusMessage(object):
    # duration — duration of message
    # is_process — whether to show dots in message
    # is_ok — whether to show ✔ (True) or ✘ (False)
    # Note, that is is_ok is not None, dots will not be shown (is_process is ignored)
    def __init__(self, msg, duration = 3, timeout = 300, priority = 0, is_process = True, is_ok = None):
        self.msg = msg
        self.duration = duration
        self.timeout = timeout
        self.priority = priority
        self.is_process = is_process
        self.is_ok = is_ok

    def is_active(self):
        if self.is_process:
            return self.timeout >= 0
        else:
            return self.duration >= 0

    def tick(self, interval):
        if self.is_process:
            self.timeout = self.timeout - interval
        else:
            self.duration = self.duration - interval
        return self.is_active()

    # Get message with dots or marks
    def message(self, ticks):
        if self.is_ok is not None:
            return u'{0}{1}'.format(self.msg, u' \u2714' if self.is_ok else u' \u2718')
        if self.is_process:
            return u'{0}{1}'.format(self.msg, '.' * (ticks % 4))
        return self.msg

    def change_message(self, new_msg):
        self.msg = new_msg

    def ok(self):
        self.is_ok = True

    def fail(self):
        self.is_ok = False

    def stop(self, is_ok = None):
        if is_ok is not None:
            self.is_ok = is_ok
        self.is_process = False

    def process(msg, timeout = 300, duration = 3, priority = 0):
        return StatusMessage(msg, duration = duration, timeout = timeout, priority = priority)

    def status(msg, duration = 3, priority = 0, is_ok = None):
        return StatusMessage(msg, duration = duration, priority = priority, is_process = False, is_ok = is_ok)

class StatusMessagesManager(threading.Thread):
    # msg ⇒ StatusMessage
    messages = LockedObject({})
    # [StatusMessage × time]
    priorities = LockedObject([])

    def __init__(self):
        super(StatusMessagesManager, self).__init__()
        self.daemon = True
        self.interval = 0.5
        self.event = threading.Event()
        self.ticks = 0
        self.timer = None

    def run(self):
        while True:
            try:
                self.event.wait(60.0)
                self.event.clear()
                self.ticks = 0
                # Ok, there are some messages, start showing them
                while self.show():
                    self.timer = threading.Timer(self.interval, self.tick)
                    self.timer.start()
                    self.timer.join()
            except Exception as e:
                log('Exception in status message: {0}'.format(e), log_error)

    def show(self):
        # Show current message, clear event if no events
        with StatusMessagesManager.priorities as ps:
            if not ps:
                return False
            else:
                cur_msg, _ = ps[0]
                sublime_status_message(cur_msg.message(self.ticks))
                return True

    def tick(self):
        self.ticks = self.ticks + 1
        # Tick all messages, remove outdated, resort priority list
        with StatusMessagesManager.priorities as ps:
            for p in ps:
                p[0].tick(self.interval)
        self.update()

    def add(self, new_message):
        with StatusMessagesManager.priorities as ps:
            ps.append((new_message, time.clock()))
        with StatusMessagesManager.messages as ms:
            ms[new_message.msg] = new_message
        self.update()
        self.event.set()

    def get(self, key):
        with StatusMessagesManager.messages as ms:
            return ms.get(key)

    def update(self):
        # Update priority list
        with StatusMessagesManager.priorities as ps:
            ps[:] = list(filter(lambda p: p[0].is_active(), ps))
            # Ended processes goes first, then by priority, and then by time of message addition
            ps.sort(key = lambda x: (x[0].is_process, -x[0].priority, x[1]))
        with StatusMessagesManager.messages as ms:
            ums = dict(filter(lambda m: m[1].is_active(), ms.items()))
            ms.clear()
            ms.update(ums)

status_message_manager = None

def show_status_message(msg, is_ok = None, priority = 0):
    """
    Show status message with check mark (is_ok = true), ballot x (is_ok = false) or ... (is_ok = None)
    """
    status_message_manager.add(StatusMessage.status(msg, priority = priority, is_ok = is_ok))

def show_status_message_process(msg, is_ok = None, timeout = 300, priority = 0):
    """
    Same as show_status_message, but shows permanently until called with is_ok not None
    There can be only one message process in time, message with highest priority is shown
    For example, when building project, there must be only message about building
    """
    if is_ok is not None:
        m = status_message_manager.get(msg)
        if m:
            m.stop(is_ok = is_ok)
    else:
        status_message_manager.add(StatusMessage.process(msg, timeout = timeout, priority = priority))

def is_cabal_source(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)

    if not window or not view:
        return False

    syntax_file_for_view = view.settings().get('syntax').lower()
    if not syntax_file_for_view.endswith("Cabal.tmLanguage".lower()):
        return False

    return True

def is_haskell_source(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)

    if not window or not view:
        return False

    syntax_file_for_view = view.settings().get('syntax').lower()
    if not syntax_file_for_view.endswith("Haskell.tmLanguage".lower()):
        return False

    return True

def is_haskell_repl(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)

    if not window or not view:
        return False

    syntax_file_for_view = view.settings().get('syntax').lower()
    if not syntax_file_for_view.endswith("HaskellRepl.tmLanguage".lower()):
        return False

    return True

def is_haskell_symbol_info(view = None):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)

    if not window or not view:
        return False

    syntax_file_for_view = view.settings().get('syntax').lower()
    if not syntax_file_for_view.endswith("HaskellSymbolInfo.tmLanguage".lower()):
        return False

    return True

class with_status_message(object):
    def __init__(self, msg, is_ok):
        self.msg = msg
        self.is_ok = is_ok

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, type, value, traceback):
        if type:
            self.fail()
        self.stop()

    def start(self):
        status_message_manager.add(self.msg)

    def stop(self):
        self.msg.stop(self.is_ok)

    def ok(self):
        self.is_ok = True

    def fail(self):
        self.is_ok = False

    def change_message(self, new_msg):
        self.msg.change_message(new_msg)

    def percentage_message(self, current, total = 100):
        self.change_message('{0} ({1}%)'.format(self.msg, int(current * 100 / total)))

def status_message(msg, is_ok = True, priority = 0):
    return with_status_message(StatusMessage.status(msg, priority = priority), is_ok = is_ok)

def status_message_process(msg, is_ok = True, timeout = 300, priority = 0):
    return with_status_message(StatusMessage.process(msg, timeout = timeout, priority = priority), is_ok = is_ok)

def sublime_haskell_package_path():
    """Get the path to where this package is installed"""
    return os.path.dirname(os.path.realpath(__file__))

def sublime_haskell_cache_path():
    """Get the path where compiled tools and caches are stored"""
    return os.path.join(sublime_haskell_package_path(), os.path.expandvars(get_setting('cache_path', '.')))

def plugin_loaded():
    package_path = sublime_haskell_package_path()
    cache_path = sublime_haskell_cache_path()

    global status_message_manager
    if not status_message_manager:
        status_message_manager = StatusMessagesManager()
        status_message_manager.start()

    if not os.path.exists(cache_path):
        os.makedirs(cache_path)

    preload_settings()

if int(sublime.version()) < 3000:
    plugin_loaded()

def create_process(command, **kwargs):
    if subprocess.mswindows:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        kwargs['startupinfo'] = startupinfo

    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stdin=subprocess.PIPE,
        stderr=subprocess.PIPE,
        shell=False,
        universal_newlines=True,
        **kwargs)

    return process

class SublimeHaskellWindowCommand(sublime_plugin.WindowCommand):
    def is_enabled(self):
        return is_enabled_haskell_command(None, False)
    def is_visible(self):
        return is_enabled_haskell_command(None, False)

class SublimeHaskellTextCommand(sublime_plugin.TextCommand):
    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)
    def is_visible(self):
        return is_enabled_haskell_command(self.view, False)
