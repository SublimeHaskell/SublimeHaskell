import fnmatch
import os
import sublime
import sublime_plugin
import subprocess

class HaskellErrorChecker(sublime_plugin.EventListener):
    def on_post_save(self, view):
        is_haskell_file = does_view_contain_haskell_source(view)
        log('view is Haskell? ' + str(is_haskell_file))
        cabal_file_path = get_cabal_file_of_view(view)
        log('path of cabal file of view: ' + str(cabal_file_path))
        # If the edited file was Haskell code within a cabal project, try to 
        # compile it.
        if is_haskell_file and cabal_file_path is not None:
            project_dir = os.path.dirname(cabal_file_path)
            error_messages = cabal_build(project_dir)
            self.write_output(view, error_messages)

    def write_output(self, view, text):
        PANEL_NAME = 'haskell_error_checker'
        output_view = view.window().get_output_panel(PANEL_NAME)
        edit = output_view.begin_edit()
        output_view.insert(edit, 0, text)
        output_view.end_edit(edit)
        view.window().run_command('show_panel', {'panel': 'output.' + PANEL_NAME})

def does_view_contain_haskell_source(view):
    "Return True if the specified view is displaying Haskell source code."
    syntax_file_for_view = view.settings().get('syntax').lower()
    return ('haskell' in syntax_file_for_view)

def get_cabal_file_of_view(view):
    "Return the path to the .cabal file above the file shown by the specified view."
    file_shown_in_view = view.file_name()
    if file_shown_in_view is None:
        return None
    return find_file_in_parent_dir(os.path.dirname(file_shown_in_view), '*.cabal')

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

def cabal_build(dir):
    "Run 'cabal build' in the specified directory and return the error output."
    process = subprocess.Popen(
        ['cabal', 'build'],
        cwd=dir,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    exit_code = process.wait()
    return stderr

def log(message):
    print('[hs-errcheck] ' + message)
