import fnmatch
import os
import sublime
import subprocess

# The path to where this package is installed:
PACKAGE_PATH = os.path.join(sublime.packages_path(), 'SublimeHaskell')

def call_and_wait(command, **popen_kwargs):
    """Run the specified command, block until it completes, and return
    the exit code, stdout, and stderr.
    Additional parameters to Popen can be specified as keyword parameters."""
    if subprocess.mswindows:
        startupinfo = subprocess.STARTUPINFO()
        startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        popen_kwargs['startupinfo'] = startupinfo
    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        **popen_kwargs)
    stdout, stderr = process.communicate()
    exit_code = process.wait()
    return (exit_code, stdout, stderr)

def log(message):
    print('Sublime Haskell: {0}'.format(message))

def get_cabal_project_dir_of_view(view):
    """Return the path to the .cabal file project for the source file in the
    specified view. The view must show a saved file, the file must be Haskell
    source code, and the file must be under a directory containing a .cabal file.
    Otherwise, return None.
    """
    # Check that the view is showing a saved file:
    file_shown_in_view = view.file_name()
    if file_shown_in_view is None:
        return None
    # Check that the file is Haskell source code:
    syntax_file_for_view = view.settings().get('syntax').lower()
    if 'haskell' not in syntax_file_for_view:
        return None
    return get_cabal_project_dir_of_file(file_shown_in_view)

def get_cabal_project_dir_of_file(filename):
    """Return the path to the .cabal file project for the specified file."""
    # Check that a .cabal file is present:
    directory_of_file = os.path.dirname(filename)
    cabal_file_path = find_file_in_parent_dir(directory_of_file, '*.cabal')
    if cabal_file_path is None:
        return None
    # Return the directory containing the .cabal file:
    return os.path.dirname(cabal_file_path)

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
    return get_settings().get(key, default)
