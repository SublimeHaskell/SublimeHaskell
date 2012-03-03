import fnmatch
import functools
import os
import re
import sublime
import sublime_plugin
import subprocess
from threading import Thread
import time

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# The first line is divided into a filename, a line number, and a column.
error_output_regex = re.compile(
    r'^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)',
    re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
result_file_regex = r'^(\S*?): line (\d+), column (\d+): (.*)$'

class HaskellErrorChecker(sublime_plugin.EventListener):
    def on_post_save(self, view):
        is_haskell_file = does_view_contain_haskell_source(view)
        cabal_file_path = get_cabal_file_of_view(view)
        # If the edited file was Haskell code within a cabal project, try to 
        # compile it.
        if is_haskell_file and cabal_file_path is not None:
            project_dir = os.path.dirname(cabal_file_path)
            cabal_process = cabal_build(project_dir)
            # On another thread, wait for the build to finish.
            write_output(view, 'Rebuilding...', project_dir)
            thread_run = functools.partial(wait_for_build_to_complete,
                view, cabal_process, project_dir)
            Thread(target=thread_run).start()

class ErrorMessage(object):
    "Describe an error or warning message produced by GHC."
    def __init__(self, filename, line, column, message):
        self.filename = filename
        self.line = line
        self.column = column
        self.message = message
        self.is_warning = 'warning' in message.lower()

    def __str__(self):
        return '{0}: line {1}, column {2}: {3}'.format(
            self.filename,
            self.line,
            self.column,
            self.message)

def wait_for_build_to_complete(view, proc, cabal_project_dir):
    "Wait for the build to complete, then parse and diplay the resulting errors."
    stdout, stderr = proc.communicate()
    exit_code = proc.wait()
    # The process has terminated; parse and display the output:
    error_messages = '\n'.join([str(x) for x in parse_error_messages(stderr)])
    success_message = 'SUCCEEDED' if exit_code == 0 else 'FAILED'
    output = '{0}\n\nBuild {1}'.format(error_messages, success_message)
    # Use set_timeout() so that the call occurs on the main Sublime thread:
    callback = functools.partial(write_output, view, output, cabal_project_dir)
    sublime.set_timeout(callback, 0)

def write_output(view, text, cabal_project_dir):
    "Write text to Sublime's output panel."
    PANEL_NAME = 'haskell_error_checker'
    output_view = view.window().get_output_panel(PANEL_NAME)
    output_view.set_read_only(False)
    # Configure Sublime's error message parsing:
    output_view.settings().set("result_file_regex", result_file_regex)
    output_view.settings().set("result_base_dir", cabal_project_dir)
    # Write to the output buffer:
    edit = output_view.begin_edit()
    output_view.insert(edit, 0, text)
    output_view.end_edit(edit)
    # Set the selection to the beginning of the view so that "next result" works:
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0))
    output_view.set_read_only(True)
    # Show the results panel:
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
    "Start 'cabal build' in the specified directory and return the running process."
    startupinfo = subprocess.STARTUPINFO()
    startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
    process = subprocess.Popen(
        ['cabal', 'build'],
        cwd=dir,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        startupinfo=startupinfo)
    return process

def parse_error_messages(text):
    "Parse text into a list of ErrorMessage objects."
    matches = error_output_regex.finditer(text)
    messages = []
    for m in matches:
        filename, line, column, messy_details = m.groups()
        messages.append(ErrorMessage(
            filename,
            line,
            column,
            clean_whitespace(messy_details)))
    return messages

def clean_whitespace(text):
    """Remove leading and trailing whitespace, plus replaces any interior 
    whitespace with a single space."""
    text = text.strip()
    text = re.sub('\s+', ' ', text)
    return text

def log(message):
    print('[hs-errcheck] ' + message)
