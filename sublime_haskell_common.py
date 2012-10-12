import errno
import fnmatch
import os
import sublime
import subprocess

# Maximum seconds to wait for window to appear
# This dirty hack is used in wait_for_window function
MAX_WAIT_FOR_WINDOW = 10

# The path to where this package is installed:
PACKAGE_PATH = os.path.join(sublime.packages_path(), 'SublimeHaskell')

# Panel for SublimeHaskell errors
SUBLIME_ERROR_PANEL_NAME = 'haskell_sublime_load'

# Setting can't be get from not main threads
# So we using a trick:
# Once setting loaded from main thread, it also stored in sublime_haskell_settings dictionary
# and callback attached to update its value
# And then setting can be get from any thread with get_setting_async
# But setting must be loaded at least once from main thread
# Some settings are loaded only from secondary threads, so we loading them here for first time
class SublimeHaskellSettingsLoader:
    def __init__(self):
        # Now we can use get_setting_async for 'add_to_PATH' safely
        get_setting('add_to_PATH')

# SublimeHaskell settings dictionary
# used to retrieve it async from any thread
sublime_haskell_settings = {}

def call_and_wait(command, **popen_kwargs):
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
        env=extended_env,
        **popen_kwargs)
    stdout, stderr = process.communicate()
    exit_code = process.wait()
    return (exit_code, stdout, stderr)

def log(message):
    print('Sublime Haskell: {0}'.format(message))

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

def get_settings():
    return sublime.load_settings("SublimeHaskell.sublime-settings")

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
    if key not in sublime_haskell_settings:
        # Load it in main thread, but for now all we can do is result default
        sublime.set_timeout(lambda: update_setting(key), 0)
        return default
    return sublime_haskell_settings[key]

def call_ghcmod_and_wait(arg_list):
    """
    Calls ghc-mod with the given arguments.
    Shows a sublime error message if ghc-mod is not available.
    """
    try:
        exit_code, out, err = call_and_wait(['ghc-mod'] + arg_list)

        if exit_code != 0:
            raise Exception("ghc-mod exited with status %d and stderr: %s" % (exit_code, err))

        return out

    except OSError, e:
        if e.errno == errno.ENOENT:
            sublime.error_message("SublimeHaskell: ghc-mod was not found!\n"
                + "It is used for LANGUAGE and import autocompletions "
                + "and type inference.\n"
                + "Try adjusting the 'add_to_PATH' setting.\n"
                + "You can also turn this off using the 'enable_ghc_mod' setting.")

def wait_for_window(on_appear, seconds_to_wait = MAX_WAIT_FOR_WINDOW):
    """
    Wait for window to appear on startup
    It's dirty hack, but I have no idea how to make it better
    """
    window = sublime.active_window()
    if window:
        on_appear(window)
        return
    if seconds_to_wait == 0:
        return
    sublime.set_timeout(lambda: wait_for_window(on_appear, seconds_to_wait - 1), 1000)

def output_error(window, text):
    "Write text to Sublime's output panel with important information about SublimeHaskell error during load"
    output_view = window.get_output_panel(SUBLIME_ERROR_PANEL_NAME)
    output_view.set_read_only(False)

    edit = output_view.begin_edit()
    output_view.insert(edit, 0, text)
    output_view.end_edit(edit)

    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0))
    output_view.set_read_only(True)

    window.run_command('show_panel', {'panel': 'output.' + SUBLIME_ERROR_PANEL_NAME})