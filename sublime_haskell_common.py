# -*- coding: UTF-8 -*-

import fnmatch
import os
import os.path
import re
import threading
import time

import sublime
import sublime_plugin

import SublimeHaskell.internals.locked_object as LockedObject

# Maximum seconds to wait for window to appear
# This dirty hack is used in wait_for_window function
MAX_WAIT_FOR_WINDOW = 10

DEFAULT_PANEL_NAME = 'sublime_haskell_panel'
# Panel for SublimeHaskell errors
ERROR_PANEL_NAME = 'sublime_haskell_error_panel'

WORD_RE = re.compile(r'^(?P<word>[\w\d\'\.]*)(?P<tail>.*)')
# Get symbol qualified prefix and its name
SYMBOL_RE = re.compile(r'((?P<module>[A-Z][\w\d]*(\.[A-Z][\w\d\']*)*)\.)?' + \
                       r'((?P<identifier>(\w[\w\d\']*)?)|(?P<operator>[!#$%&*+\./<=>?@\\\^|\-~:]+))$')
# Get import name
IMPORT_MODULE_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)\b')
# SYMBOL_RE = re.compile(r'((?P<module>\w+(\.\w+)*)\.)?(?P<identifier>((\w*)|([]*)))$')
# Get symbol module scope and its name within import statement
IMPORT_SYMBOL_RE = re.compile(r'import(\s+qualified)?\s+(?P<module>[A-Z][\w\d\']*(\.[A-Z][\w\d\']*)*)' + \
                              r'(\s+as\s+(?P<as>[A-Z][\w\d\']*))?\s*' + \
                              r'\(.*?((?P<identifier>([a-z][\w\d\']*)?)|(\((?P<operator>[!#$%&*+\.\/<=>?@\\\^|\-~:]*)))$')


# Original signature had: must_be_main=False, must_be_file=False
# This function is never called with either of these two keywords. In fact, it is never called with keywords.
def is_enabled_haskell_command(view, must_be_project):
    """Returns True if command for .hs can be invoked"""
    window, view = get_haskell_command_window_view_file_project(view)[0:2]

    if not window or not view:
        return False

    # Note: file_show_in_view is the third element of the get_haskell_command_window_view_file_project() return tuple.
    # if must_be_file and not file_shown_in_view:
    #     return False

    syntax_file_for_view = view.settings().get('syntax')
    if not syntax_file_for_view or ('haskell' not in syntax_file_for_view.lower()):
        return False

    if not must_be_project:
        return True

    cabal_project_dir = get_cabal_project_dir_of_view(view)
    if not cabal_project_dir:
        return False
    return True


def get_haskell_command_window_view_file_project(view=None):
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


def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, _dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files


def get_cwd(filename=None):
    """
    Get cwd for filename: cabal project path, file path or os.getcwd()
    """
    cwd = (get_cabal_project_dir_of_file(filename) or os.path.dirname(filename)) if filename else os.getcwd()
    return cwd


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
    def run(self, edit, **kwargs):
        text = kwargs.get('text')
        clear = kwargs.get('clear')

        if text:
            self.view.set_read_only(False)
            if clear:
                self.view.erase(edit, sublime.Region(0, self.view.size()))
            self.view.insert(edit, self.view.size(), text)
            self.view.set_read_only(True)


class SublimeHaskellReplaceText(sublime_plugin.TextCommand):
    """
    Helper command to replace region
    """
    def __init__(self, view):
        super().__init__(view)

    def run(self, edit, **kwargs):
        text = kwargs.get('text')
        begin = kwargs.get('begin')
        end = kwargs.get('end')

        if text is not None and begin is not None and end is not None:
            read_only = self.view.is_read_only()
            self.view.set_read_only(False)
            self.view.replace(edit, sublime.Region(begin, end), text)
            self.view.set_read_only(read_only)


# Output some text to view (panel), possibly clearing
def output_text(view, text=None, clear=False):
    view.run_command('sublime_haskell_output_text', {'text': (text or ''), 'clear': 'yes' if clear else ''})


# Create new output panel
def output_panel(window, text='', panel_name=DEFAULT_PANEL_NAME, syntax=None, panel_display=True):
    output_view = None
    if window is not None:
        output_view = window.get_output_panel(panel_name)
        if syntax is not None:
            output_view.set_syntax_file('Packages/SublimeHaskell/Syntaxes/{0}.tmLanguage'.format(syntax))
        output_text(output_view, text, clear=True)
        output_view.sel().clear()
        output_view.sel().add(sublime.Region(0, 0))
        if panel_display:
            window.run_command('show_panel', {'panel': ('output.' + panel_name)})

    return output_view


def hide_panel(window, panel_name=DEFAULT_PANEL_NAME):
    if not window:
        window = sublime.active_window()
    if not window:
        return
    window.run_command('hide_panel', {'panel': ('output.' + panel_name)})


def show_panel(window, panel_name=DEFAULT_PANEL_NAME):
    if window is None:
        window = sublime.active_window()
        if window is None:
            return
    window.run_command('show_panel', {'panel': ('output.' + panel_name)})


def output_error(window, text):
    output_panel(window, text, panel_name=ERROR_PANEL_NAME)


def output_error_async(window, text):
    sublime.set_timeout(lambda: output_error(window, text), 0)


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

    # (before, at, after)
    return (view.substr(sublime.Region(line_region.begin(), region.begin())),
            view.substr(region),
            view.substr(sublime.Region(region.end(), line_region.end())))


def get_line_contents_before_region(view, region):
    """
    Returns contents of line before the given region (including it).
    """
    (before, point, _) = get_line_contents_at_region(view, region)
    return before + point


def get_qualified_name(name):
    """
    'bla bla bla Data.List.fo' -> ('Data.List', 'Data.List.fo')
    """
    if len(name) == 0:
        return ('', '')
    quals = name.split()[-1].split('.')
    filtered = list(map(lambda s: list(filter(lambda c: c.isalpha() or c.isdigit() or c == '_', s)), quals))
    return ('.'.join(filtered[0:len(filtered) - 1]), '.'.join(filtered))


class QualifiedSymbol(object):
    def __init__(self, name=None, module=None, module_as=None, is_import_list=False, is_operator=False):
        self.name = name
        self.module = module
        self.module_as = module_as
        self.is_import_list = is_import_list
        self.is_operator = is_operator

    def __str__(self):
        return u'QualifiedSymbol(name: {0}, module {1}, '.format(self.name, self.module) + \
               u'module_as {0}, is_import_list {1}, is_operator {2}'.format(self.module_as, self.is_import_list,
                                                                            self.is_operator)

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
        return QualifiedSymbol(name=next(i for i in [res.group('identifier'), res.group('operator')] if i is not None),
                               module=res.group('module'),
                               module_as=res.group('as'),
                               is_import_list=True,
                               is_operator=bool(res.group('operator')))
    res = IMPORT_MODULE_RE.search(line)
    if res:
        return QualifiedSymbol(module=res.group('module'))
    res = SYMBOL_RE.search(line)
    # res always match
    return QualifiedSymbol(module=res.group('module'),
                           name=next(i for i in [res.group('identifier'), res.group('operator')] if i is not None),
                           is_operator=bool(res.group('operator')))


def get_qualified_symbol_at_region(view, region):
    """
    Get module context of symbol and symbol itself for line before (and with) word on region
    Returns (module, name), where module (or one of) can be None
    """
    (before, point, after) = get_line_contents_at_region(view, region)
    res = WORD_RE.match(after)
    if res:
        point = point + res.group('word')
    return get_qualified_symbol(before + point)


def get_qualified_symbol_at_point(view, point):
    return get_qualified_symbol_at_region(view, sublime.Region(point, point))


def sublime_status_message(msg):
    """
    Pure msg with 'SublimeHaskell' prefix and set_timeout
    """
    sublime.set_timeout(lambda: sublime.status_message(u'SublimeHaskell: {0}'.format(msg)), 0)


class StatusMessage(object):
    # duration — duration of message
    # is_process — whether to show dots in message
    # is_ok — whether to show ✔ (True) or ✘ (False)
    # Note, that is is_ok is not None, dots will not be shown (is_process is ignored)
    def __init__(self, msg, duration=1, timeout=300, priority=0, is_process=True, is_ok=None):
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

    def stop(self, is_ok=None):
        if is_ok is not None:
            self.is_ok = is_ok
        self.is_process = False

    @staticmethod
    def process(msg, timeout=300, duration=1, priority=0):
        return StatusMessage(msg, duration=duration, timeout=timeout, priority=priority)

    @staticmethod
    def status(msg, duration=1, priority=0, is_ok=None):
        return StatusMessage(msg, duration=duration, priority=priority, is_process=False, is_ok=is_ok)


class StatusMessagesManager(threading.Thread):
    # msg ⇒ StatusMessage
    messages = LockedObject.LockedObject({})
    # [StatusMessage × time]
    priorities = LockedObject.LockedObject([])

    def __init__(self):
        super().__init__()
        self.daemon = True
        self.interval = 0.1
        self.event = threading.Event()
        self.ticks = 0
        self.timer = None

    def run(self):
        while True:
            self.event.wait(60.0)
            self.event.clear()
            self.ticks = 0
            # Ok, there are some messages, start showing them
            while self.show():
                self.timer = threading.Timer(self.interval, self.tick)
                self.timer.start()
                self.timer.join()

    def show(self):
        # Show current message, clear event if no events
        with StatusMessagesManager.priorities as prios:
            if not prios:
                return False
            else:
                cur_msg, _ = prios[0]
                sublime_status_message(cur_msg.message(self.ticks))
                return True

    def tick(self):
        self.ticks = self.ticks + 1
        # Tick all messages, remove outdated, resort priority list
        with StatusMessagesManager.priorities as prios:
            for prio in prios:
                prio[0].tick(self.interval)
        self.update()

    def add(self, new_message):
        with StatusMessagesManager.priorities as prios:
            prios.append((new_message, time.clock()))
        with StatusMessagesManager.messages as msgs:
            msgs[new_message.msg] = new_message
        self.update()
        self.event.set()

    def get(self, key):
        with StatusMessagesManager.messages as msgs:
            return msgs.get(key)

    def update(self):
        # Update priority list
        with StatusMessagesManager.priorities as prios:
            prios[:] = list(filter(lambda p: p[0].is_active(), prios))
            # Ended processes goes first, then by priority, and then by time of message addition
            prios.sort(key=lambda x: (x[0].is_process, -x[0].priority, x[1]))
        with StatusMessagesManager.messages as msgs:
            ums = dict(filter(lambda m: m[1].is_active(), msgs.items()))
            msgs.clear()
            msgs.update(ums)

# Iff STATUS_MSG_MANAGER hasn't been defined in the module yet (globals()), then go ahead and create it.
if 'STATUS_MSG_MANAGER' not in globals():
    STATUS_MSG_MANAGER = StatusMessagesManager()
    STATUS_MSG_MANAGER.start()
else:
    STATUS_MSG_MANAGER = globals()['STATUS_MSG_MANAGER']

def show_status_message(msg, is_ok=None, priority=0):
    """
    Show status message with check mark (is_ok = true), ballot x (is_ok = false)
    """
    STATUS_MSG_MANAGER.add(StatusMessage.status(msg, priority=priority, is_ok=is_ok))


def show_status_message_process(msg, is_ok=None, timeout=300, priority=0):
    """
    Same as show_status_message, but shows permanently until called with is_ok not None
    There can be only one message process in time, message with highest priority is shown
    For example, when building project, there must be only message about building
    """
    if is_ok is not None:
        smsg = STATUS_MSG_MANAGER.get(msg)
        if smsg:
            smsg.stop(is_ok=is_ok)
    else:
        STATUS_MSG_MANAGER.add(StatusMessage.process(msg, timeout=timeout, priority=priority))


def is_with_syntax(view=None, syntax=None):
    if syntax is None:
        return False

    window, view = get_haskell_command_window_view_file_project(view)[0:2]

    if not window or not view:
        return False

    syntax_file_for_view = view.settings().get('syntax')
    if not syntax_file_for_view or not syntax_file_for_view.lower().endswith(syntax.lower()):
        return False

    return True


def is_cabal_source(view=None):
    return is_with_syntax(view, syntax="Cabal.tmLanguage")


def is_haskell_source(view=None):
    return is_with_syntax(view, syntax="Haskell.tmLanguage")


def is_inspected_source(view=None):
    return is_haskell_source(view) or is_cabal_source(view)


def is_haskell_repl(view=None):
    return is_with_syntax(view, syntax="HaskellRepl.tmLanguage")


def is_haskell_symbol_info(view=None):
    return is_with_syntax(view, syntax="HaskellSymbolInfo.tmLanguage")


class StatusMessageContext(object):
    def __init__(self, msg, is_ok):
        self.msg = msg
        self.is_ok = is_ok

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        if exc_type:
            self.fail()
        self.stop()

    def start(self):
        STATUS_MSG_MANAGER.add(self.msg)

    def stop(self):
        self.msg.stop(self.is_ok)

    def ok(self):
        self.is_ok = True

    def fail(self):
        self.is_ok = False

    def change_message(self, new_msg):
        self.msg.change_message(new_msg)

    def percentage_message(self, current, total=100):
        self.change_message('{0} ({1}%)'.format(self.msg, int(current * 100 / total)))


def status_message(msg, is_ok=True, priority=0):
    return StatusMessageContext(StatusMessage.status(msg, priority=priority), is_ok=is_ok)


def status_message_process(msg, is_ok=True, timeout=300, priority=0):
    return StatusMessageContext(StatusMessage.process(msg, timeout=timeout, priority=priority), is_ok=is_ok)


def sublime_haskell_cache_path():
    """Get the path where compiled tools and caches are stored"""
    return os.path.join(sublime.cache_path(), 'SublimeHaskell')


class SublimeHaskellWindowCommand(sublime_plugin.WindowCommand):
    def __init__(self, view):
        super().__init__(view)

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)

    def is_visible(self):
        return is_enabled_haskell_command(None, False)


class SublimeHaskellTextCommand(sublime_plugin.TextCommand):
    def __init__(self, view):
        super().__init__(view)

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)

    def is_visible(self):
        return is_enabled_haskell_command(self.view, False)
