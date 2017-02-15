# -*- coding: UTF-8 -*-

import errno
import fnmatch
import os
import os.path
import re
import json
import html
import platform
import string
import sublime
import sublime_plugin
import subprocess
import threading
import time
from sys import version_info, stdout, stderr

PyV3 = version_info >= (3,)

# Maximum seconds to wait for window to appear
# This dirty hack is used in wait_for_window function
MAX_WAIT_FOR_WINDOW = 10

DEFAULT_PANEL_NAME = 'sublime_haskell_panel'
# Panel for SublimeHaskell errors
ERROR_PANEL_NAME = 'sublime_haskell_error_panel'

WORD_RE = re.compile(r'^(?P<word>[\w\d\'\.]*)(?P<tail>.*)')
# Get symbol qualified prefix and its name
SYMBOL_RE = re.compile(r'((?P<module>[A-Z][\w\d]*(\.[A-Z][\w\d\']*)*)\.)?((?P<identifier>(\w[\w\d\']*)?)|(?P<operator>[!#$%&*+\./<=>?@\\\^|\-~:]+))$')
# Get import name
IMPORT_MODULE_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)\b')
# SYMBOL_RE = re.compile(r'((?P<module>\w+(\.\w+)*)\.)?(?P<identifier>((\w*)|([]*)))$')
# Get symbol module scope and its name within import statement
IMPORT_SYMBOL_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)(\s+as\s+(?P<as>[A-Z][\w\d\']*))?\s*\(.*?((?P<identifier>([a-z][\w\d\']*)?)|(\((?P<operator>[!#$%&*+\.\/<=>?@\\\^|\-~:]*)))$')


def python3():
    return PyV3


def isWinXX():
    return platform.system() == "Windows"


# Logging primitives
log_error = 1
log_warning = 2
log_info = 3
log_debug = 4
log_trace = 5


def log(message, level = log_info):
    log_level = get_setting_async('log', log_info)
    if log_level >= level:
        print(u'Sublime Haskell: {0}'.format(message))


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


# SublimeHaskell settings dictionary
# used to retrieve it async from any thread
sublime_haskell_settings = LockedObject({})
# Callbacks for updated settings/change detection
sublime_settings_changes = LockedObject({})

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
        return lambda: on_changed_setting(str(key))

    for key in [ 'add_standard_dirs'
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
               ]:
        get_setting(key)
        with sublime_haskell_settings as settings:
            get_settings().add_on_change(str(key), change_func(key))
            with sublime_settings_changes as changes:
                changes[key] = []

    # Register change detection:
    with sublime_settings_changes as changes:
        changes['add_to_PATH'].append(ProcHelper.update_environment)
        changes['add_standard_dirs'].append(ProcHelper.update_environment)


def is_enabled_haskell_command(view = None, must_be_project=True, must_be_main=False, must_be_file = False):
    """Returns True if command for .hs can be invoked"""
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)

    if not window or not view:
        return False

    if must_be_file and not file_shown_in_view:
        return False

    syntax_file_for_view = view.settings().get('syntax')
    if not syntax_file_for_view or ('haskell' not in syntax_file_for_view.lower()):
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


def head_of(l):
    if l is None or not len(l):
        return None
    return l[0]


def decode_bytes(s):
    if s is None:
        return None
    return s if PyV3 else s.decode('utf-8')


def encode_bytes(s):
    if s is None:
        return None
    return s if PyV3 else s.encode('utf-8')


def tool_enabled(feature):
    return 'enable_{0}'.format(feature)


class ProcHelper(object):
    """Command and tool process execution helper."""

    # Tool name -> executable path cache. Avoids probing the file system multiple times.
    which_cache = { }
    # Augmented environment for the subprocesses. Specifically, we really want
    # to augment the user's PATH used to search for executables and tools:
    augmented_env = None

    def __init__(self, command, input_string = '', **popen_kwargs):
        """Open a pipe to a command or tool."""

        if ProcHelper.augmented_env is None:
            ProcHelper.augmented_env = ProcHelper.get_extended_env()

        self.process = None
        self.process_err = None

        if subprocess.mswindows:
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            popen_kwargs['startupinfo'] = startupinfo

        try:
            normcmd = ProcHelper.which(command, ProcHelper.augmented_env['PATH'])
            if normcmd is not None:
                self.process = subprocess.Popen(normcmd
                                               , stdout=subprocess.PIPE
                                               , stderr=subprocess.PIPE
                                               , stdin=subprocess.PIPE
                                               , env=ProcHelper.augmented_env
                                               , universal_newlines=True
                                               , **popen_kwargs
                                               )

                self.process.stdin.write(encode_bytes(input_string))
                self.process.stdin.flush()
            else:
                self.process = None
                self.process_err = "SublimeHaskell.ProcHelper: {0} was not found on PATH!".format(command[0])

        except OSError as e:
            self.process = None
            self.process_err = "SublimeHaskell: {0} was not found!\n'{1}' is set to False".format(tool_name, tool_enabled(tool_name))
            if e.errno == errno.ENOENT:
                # Just paranoia
                self.cleanup()

            # Other consumers want this exception
            raise e

    # 'with' statement support:
    def __enter__(self):
        return self

    def __exit__(self, type, value, traceback):
        self.cleanup()
        return False

    def cleanup(self):
        if self.process is not None:
            self.process.stdin.close()
            self.process.stdout.close()
            self.process.stderr.close()

    def wait(self):
        """Wait for subprocess to complete and exit, collect and decode ``stdout`` and ``stderr``, returning the tuple
        ``(exit_code, stdout, stderr)```"""
        if self.process is not None:
            stdout, stderr = self.process.communicate()
            exit_code = self.process.wait()
            # Ensure that we reap the file descriptors.
            self.cleanup()
            return (exit_code, decode_bytes(stdout), decode_bytes(stderr))
        else:
            return (-1, '', self.process_err or "?? unknown error -- no process.")

    # Update the augmented environment when `add_to_PATH` or `add_standard_dirs` change.
    @staticmethod
    def update_environment(key, val):
        ProcHelper.augmented_env = ProcHelper.get_extended_env()

    # Generate the augmented environment for subprocesses. This copies the
    # current process environment and updates PATH with `add_to_PATH` extras.
    @staticmethod
    def get_extended_env():
        def normalize_path(dir):
            return os.path.normpath(os.path.expandvars(os.path.expanduser(dir)))

        def cabal_config():
            cconfig = os.environ.get('CABAL_CONFIG') or \
                ('~/.cabal' if not isWinXX() else '%APPDATA%/cabal') + \
                "/config"

            # Various patterns to match...
            re_user_dirs = re.compile('^install-dirs\s+user')
            re_global_dirs = re.compile('^install-dirs\s+global')
            re_section = re.compile('^\w+')
            re_prefix = re.compile('prefix:\s+(.*)$')
            re_bindir = re.compile('bindir:\s+(.*)$')

            # Things to collect
            user_prefix = "$HOME/.cabal" if not isWinXX() else "%APPDATA%/cabal"
            # FIXME: Need to interrogate Shel32 for the Windows PROGRAMFILES known
            # folder path:
            global_prefix = "/usr/local" if not isWinXX() else "%PROGRAMFILES%/Haskell"
            user_bindir = "bin"
            global_bindir = "bin"
            p_state = 0

            try:
                with open(normalize_path(cconfig), 'rU') as f_cconfig:
                    # You would think that the Cabal maintainers would use a
                    # well known file format... But now, they didn't. And they
                    # had to go with an indentation-specific format.
                    #
                    # This is a "cheap and dirty" scanner to pick up 
                    for l in f_cconfig:
                        l1 = l.rstrip()
                        # One of the sections?
                        if re_user_dirs.match(l1):
                            p_state = 1
                        elif re_global_dirs.match(l1):
                            p_state = 2
                        elif re.match('^\s+\w', l1):
                            # prefix attribute?
                            m = re_prefix.search(l1)
                            if m:
                                if p_state == 1:
                                    user_prefix = m.group(1)
                                elif p_state == 2:
                                    global_prefix = m.group(1)
                            # bindir attribute?
                            m = re_bindir.search(l1)
                            if m:
                                if p_state == 1:
                                    user_bindir = m.group(1)
                                elif p_state == 2:
                                    global_bindir = m.group(1)
                        elif re_section.match(l1):
                            p_state = 0

            except IOError as e:
                # Silently fail.
                pass

            return [ os.path.join(user_prefix, user_bindir)
                   , os.path.join(global_prefix, global_bindir)
                   ]

        ext_env = dict(os.environ)
        PATH = os.getenv('PATH') or ""
        std_places = []
        if get_setting_async('add_standard_dirs', False):
            std_places = ["$HOME/.local/bin" if not isWinXX() else "%APPDATA%/local/bin"] + cabal_config()
            std_places = list(filter(os.path.isdir, map(normalize_path, std_places)))

        add_to_PATH = list(map(normalize_path, get_setting_async('add_to_PATH', [])))
        if not PyV3:
            # convert unicode strings to strings (for Python < 3). Environment
            # can contain only strings.
            add_to_PATH = list(map(str, add_to_PATH))

        add_to_PATH = list(filter(os.path.isdir, add_to_PATH))

        print("std_places = {0}".format(std_places))
        print("add_to_PATH = {0}".format(add_to_PATH))

        ext_env['PATH'] = os.pathsep.join(add_to_PATH + std_places + [PATH])
        return ext_env

    @staticmethod
    def which(args, env_path):
        def is_exe(fpath):
            return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

        cval = ProcHelper.which_cache.get(args[0])
        if cval is not None:
            args[0] = cval
            return args
        else:
            exeExts = [''] if not isWinXX() else ['.exe', '.cmd', '.bat']

            program = args[0]
            fpath, fname = os.path.split(program)
            if fpath:
                if is_exe(program):
                    return args
            else:
                for path in env_path.split(os.pathsep):
                    path = path.strip('"')
                    for ext in exeExts:
                        exe_file = os.path.join(path, program)
                        if is_exe(exe_file + ext):
                            ProcHelper.which_cache[program] = exe_file
                            args[0] = exe_file
                            return args

        return None

    @staticmethod
    def run_process(command, input_string = '', **popen_kwargs):
        """Execute a subprocess, wait for it to complete, returning a ``(exit_code, stdout, stderr)``` tuple."""
        with ProcHelper(command, input_string, **popen_kwargs) as p:
            return p.wait()

    @staticmethod
    def invoke_tool(command, tool_name, input = '', on_result = None, filename = None, on_line = None, check_enabled = True, **popen_kwargs):
        if check_enabled and (not get_setting_async(tool_enabled(tool_name))):
            return None
        # extended_env = get_extended_env()

        source_dir = get_source_dir(filename)

        def mk_result(s):
            return on_result(s) if on_result else s

        try:
            with ProcHelper(command, input, cwd = source_dir, **popen_kwargs) as p:
                exit_code, stdout, stderr = p.wait()
                if exit_code != 0:
                    raise Exception('{0} exited with exit code {1} and stderr: {2}'.format(tool_name, exit_code, stderr))

                if on_line:
                    for l in stdout.splitlines():
                        on_line(mk_result(l))
                else:
                    return mk_result(stdout)

        except OSError as e:
            if e.errno == errno.ENOENT:
                output_error_async(sublime.active_window(), "SublimeHaskell: {0} was not found!\n'{1}' is set to False".format(tool_name, tool_enabled(tool_name)))
                set_setting_async(tool_enabled(tool_name), False)
            else:
                log('{0} fails with {1}, command: {2}'.format(tool_name, e, command), log_error)

            return None

        except Exception as e:
            log('{0} fails with {1}, command: {2}'.format(tool_name, e, command), log_error)

        return None

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


def is_stack_project(project_dir):
    """Search for stack.yaml in parent directories"""
    return find_file_in_parent_dir(project_dir, "stack.yaml") is not None


# Get stack dist path
def stack_dist_path(project_dir):
    exit_code, out, err = ProcHelper.run_process(['stack', 'path'], cwd = project_dir)
    if exit_code == 0:
        ds = [d for d in out.splitlines() if d.startswith('dist-dir: ')]
        if len(ds):
            dist_dir = ds[0][10:]
            return os.path.join(project_dir, dist_dir)


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


def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files


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


def subscribe_setting(key, fn):
    with sublime_settings_changes as changes:
        if key not in changes:
            changes[key] = []
        changes[key].append(fn)


def ghci_package_db(cabal = None):
    if not cabal or cabal == 'cabal':
        return None
    package_conf = (filter(lambda x: re.match('packages-(.*)\.conf', x), os.listdir(cabal)) + [None])[0]
    if package_conf:
        return os.path.join(cabal, package_conf)
    return None


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
    exit_code, out, err = ProcHelper.run_process(['hsinspect', cabal_file])

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
        args.extend(["-g", "\"" + opt + "\""])
    return args


def call_ghcmod_and_wait(arg_list, filename=None, cabal = None):
    """
    Calls ghc-mod with the given arguments.
    Shows a sublime error message if ghc-mod is not available.
    """

    ghc_opts_args = get_ghc_opts_args(filename, add_package_db = False, cabal = cabal)

    try:
        command = ['ghc-mod'] + ghc_opts_args + arg_list

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
        exit_code, out, err = ProcHelper.run_process(command, cwd=ghc_mod_current_dir)

        if exit_code != 0:
            raise Exception("%s exited with status %d and stderr: %s" % (' '.join(command), exit_code, err))

        # return crlf2lf(out)
        return out

    except OSError as e:
        if e.errno == errno.ENOENT:
            output_error_async(
                sublime.active_window(),
                "SublimeHaskell: ghc-mod was not found!\n"
                "It is used for LANGUAGE and import autocompletions and type inference.\n"
                "Try adjusting the 'add_to_PATH' setting.\n"
                "You can also turn this off using the 'enable_ghc_mod' setting.")
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


def use_unicode_operators(s, force = False):
    """
    Set unicode symbols for some standard haskell operators
    """
    if not force and not get_setting_async('unicode_symbol_info'):
        return s

    ops = {
        '->': '\u2192',
        '=>': '\u21d2',
        '::': '\u2237' }
    ops.update(dict((html.escape(o), v) for o, v in ops.items()))
    r = s
    for o, v in ops.items():
        r = r.replace(o, v)
    return r


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


class SublimeHaskellReplaceText(sublime_plugin.TextCommand):
    """
    Helper command to replace region
    """
    def run(self, edit, text = None, begin = None, end = None):
        if text is None or begin is None or end is None:
            return
        read_only = self.view.is_read_only()
        self.view.set_read_only(False)
        self.view.replace(edit, sublime.Region(begin, end), text)
        self.view.set_read_only(read_only)


# Output some text to view (panel), possibly clearing
def output_text(view, text = None, clear = False):
    view.run_command('sublime_haskell_output_text', {'text': (text or ''), 'clear': 'yes' if clear else ''})


# Create new output panel
def output_panel(window, text = '', panel_name = DEFAULT_PANEL_NAME, syntax = None, show_panel = True):
    if not window:
        return None
    output_view = window.get_output_panel(panel_name)
    if syntax is not None:
        output_view.set_syntax_file('Packages/SublimeHaskell/Syntaxes/{0}.tmLanguage'.format(syntax))
    output_text(output_view, text, clear = True)
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0, 0))
    if show_panel:
        window.run_command('show_panel', {'panel': ('output.' + panel_name)})
    return output_view


def hide_panel(window, panel_name = DEFAULT_PANEL_NAME):
    if not window:
        window = sublime.active_window()
    if not window:
        return
    window.run_command('hide_panel', {'panel': ('output.' + panel_name)})


def show_panel(window, panel_name = DEFAULT_PANEL_NAME):
    if not window:
        window = sublime.active_window()
    if not window:
        return
    window.run_command('show_panel', {'panel': ('output.' + panel_name)})


def output_error(window, text):
    output_panel(window, text, panel_name = ERROR_PANEL_NAME)


def output_error_async(window, text):
    sublime.set_timeout(lambda: output_error(window, text), 0)


class SublimeHaskellError(RuntimeError):
    def __init__(self, what):
        self.reason = what


def get_line_contents(view, location):
    """
    Returns contents of line at the given location.
    """
    return view.substr(sublime.Region(view.line(location).a, location))


def get_line_contents_at_region(view, region):
    """
    Returns (before, at, after)
    """
    line_region = view.line(region)

    before = view.substr(sublime.Region(line_region.begin(), region.begin()))
    at = view.substr(region)
    after = view.substr(sublime.Region(region.end(), line_region.end()))
    return (before, at, after)


def get_line_contents_before_region(view, region):
    """
    Returns contents of line before the given region (including it).
    """
    (before, at, _) = get_line_contents_at_region(view, region)
    return before + at


def get_qualified_name(s):
    """
    'bla bla bla Data.List.fo' -> ('Data.List', 'Data.List.fo')
    """
    if len(s) == 0:
        return ('', '')
    quals = s.split()[-1].split('.')
    filtered = map(lambda s: list(filter(lambda c: c.isalpha() or c.isdigit() or c == '_', s)), quals)
    return ('.'.join(filtered[0:len(filtered) - 1]), '.'.join(filtered))


class QualifiedSymbol(object):
    def __init__(self, name = None, module = None, module_as = None, is_import_list = False, is_operator = False):
        self.name = name
        self.module = module
        self.module_as = module_as
        self.is_import_list = is_import_list
        self.is_operator = is_operator

    def qualified_name(self):
        if self.name is None:
            return self.module
        return '{0}.{1}'.format(self.module_as or self.module, self.name) if self.module else self.name

    def full_name(self):
        if self.name is None:
            return self.module
        return '{0}.{1}'.format(self.module, self.name) if self.module else self.name

    def is_module(self):
        return self.name is None and self.module is not None


def get_qualified_symbol(line):
    """
    Get module context of symbol and symbol itself
    Returns (module, as, name, is_import_list, is_operator), where module (or one of) can be None
    """
    res = IMPORT_SYMBOL_RE.search(line)
    if res:
        return QualifiedSymbol(
            name = next(i for i in [res.group('identifier'), res.group('operator')] if i is not None),
            module = res.group('module'),
            module_as = res.group('as'),
            is_import_list = True,
            is_operator = bool(res.group('operator')))
    res = IMPORT_MODULE_RE.search(line)
    if res:
        return QualifiedSymbol(module = res.group('module'))
    res = SYMBOL_RE.search(line)
    # res always match
    return QualifiedSymbol(
        module = res.group('module'),
        name = next(i for i in [res.group('identifier'), res.group('operator')] if i is not None),
        is_operator = bool(res.group('operator')))


def get_qualified_symbol_at_region(view, region):
    """
    Get module context of symbol and symbol itself for line before (and with) word on region
    Returns (module, name), where module (or one of) can be None
    """
    (before, at, after) = get_line_contents_at_region(view, region)
    res = WORD_RE.match(after)
    if res:
        at = at + res.group('word')
    return get_qualified_symbol(before + at)


def get_qualified_symbol_at_point(view, point):
    return get_qualified_symbol_at_region(view, sublime.Region(point, point))


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


class StatusMessage(object):
    # duration — duration of message
    # is_process — whether to show dots in message
    # is_ok — whether to show ✔ (True) or ✘ (False)
    # Note, that is is_ok is not None, dots will not be shown (is_process is ignored)
    def __init__(self, msg, duration = 1, timeout = 300, priority = 0, is_process = True, is_ok = None):
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
            # return u'{0} {1}'.format(self.msg, u'\u2714' if self.is_ok else u'\u2718')
            return u'{0} {1}'.format(self.msg, u'[ok]' if self.is_ok else u'[error]')
        if self.is_process:
            return u'{0}{1}'.format(self.msg, '.' * (int(ticks / 5) % 4))
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

    @staticmethod
    def process(msg, timeout = 300, duration = 1, priority = 0):
        return StatusMessage(msg, duration = duration, timeout = timeout, priority = priority)

    @staticmethod
    def status(msg, duration = 1, priority = 0, is_ok = None):
        return StatusMessage(msg, duration = duration, priority = priority, is_process = False, is_ok = is_ok)


class StatusMessagesManager(threading.Thread):
    # msg ⇒ StatusMessage
    messages = LockedObject({})
    # [StatusMessage × time]
    priorities = LockedObject([])

    def __init__(self):
        super(StatusMessagesManager, self).__init__()
        self.daemon = True
        self.interval = 0.1
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
    Show status message with check mark (is_ok = true), ballot x (is_ok = false)
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


def is_with_syntax(view = None, syntax = None):
    if syntax is None:
        return False

    window, view, file_shown_in_view = get_haskell_command_window_view_file_project(view)

    if not window or not view:
        return False

    syntax_file_for_view = view.settings().get('syntax')
    if not syntax_file_for_view or not syntax_file_for_view.lower().endswith(syntax.lower()):
        return False

    return True


def is_cabal_source(view = None):
    return is_with_syntax(view, syntax = "Cabal.tmLanguage")


def is_haskell_source(view = None):
    return is_with_syntax(view, syntax = "Haskell.tmLanguage")


def is_inspected_source(view = None):
    return is_haskell_source(view) or is_cabal_source(view)


def is_haskell_repl(view = None):
    return is_with_syntax(view, syntax = "HaskellRepl.tmLanguage")


def is_haskell_symbol_info(view = None):
    return is_with_syntax(view, syntax = "HaskellSymbolInfo.tmLanguage")


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


def sublime_haskell_cache_path():
    """Get the path where compiled tools and caches are stored"""
    return os.path.join(sublime.cache_path(), 'SublimeHaskell')


def plugin_loaded():
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
