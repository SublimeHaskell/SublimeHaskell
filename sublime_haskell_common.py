# -*- coding: UTF-8 -*-

import fnmatch
import functools
import heapq
import itertools
import os
import os.path
import re
import threading

import sublime
import sublime_plugin

import SublimeHaskell.internals.settings as Settings


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
                              r'\(.*?((?P<identifier>([A-Za-z][\w\d\']*)?)|(\((?P<operator>[!#$%&*+\.\/<=>?@\\\^|\-~:]*)))$')


def is_enabled_haskell_command(view, must_be_project):
    '''Returns True if command for Haskell source can be invoked.
    '''
    window, view, _file = window_view_and_file(view)

    if not window or not view:
        return False

    view_settings = view.settings() or {}
    syntax_file_for_view = (view_settings.get('syntax') or '').lower()
    return bool('haskell' in syntax_file_for_view and (not must_be_project or get_cabal_project_dir_of_view(view)))


def window_view_and_file(view=None):
    '''Returns window, view and the view's file name. If view is None, use the active window, the window's active view.
    '''

    if view:
        return view.window(), view, view.file_name()

    window = sublime.active_window()
    view = window.active_view() if window else None
    fname = view.file_name() if view else None

    return window, view, fname

def locate_cabal_project_from_view(view):
    """Return the path to the .cabal file project for the source file in the
    specified view. The view must show a saved file, the file must be Haskell
    source code, and the file must be under a directory containing a .cabal file.
    Otherwise, return None.

    :rtype: Tuple
    :return: (project directory path, project name)
    """

    # Use cached info whenever possible.
    ## SURPRISE! These settings persist across invocations of ST! (Actually, not a bad thing.)
    vsettings = view.settings()
    projname = vsettings.get(Settings.SETTING_SUBHASK_PROJECT)
    projdir = vsettings.get(Settings.SETTING_SUBHASK_PROJDIR)
    if projname is None or projdir is None:
        # Check that the view is showing a saved file:
        projdir = None
        projname = None

        file_shown_in_view = view.file_name()
        if file_shown_in_view is not None:
            # Check that the file is Haskell source code:
            syntax_file_for_view = view.settings().get('syntax').lower()
            if 'haskell' in syntax_file_for_view:
                projdir, projname = locate_cabal_project(file_shown_in_view)
                if projdir is not None and projname is not None:
                    vsettings.set(Settings.SETTING_SUBHASK_PROJECT, projname)
                    vsettings.set(Settings.SETTING_SUBHASK_PROJDIR, projdir)

    return (projdir, projname)


def locate_cabal_project(filename):
    """Return the path to the .cabal file and name of project for the specified file."""
    # Check that a .cabal file is present:
    directory_of_file = os.path.dirname(filename)
    cabal_file_path = find_file_in_parent_dir(directory_of_file, '*.cabal')
    if cabal_file_path is not None:
        # Return the directory containing the .cabal file:
        project_path, cabal_file = os.path.split(cabal_file_path)
        project_name = os.path.splitext(cabal_file)[0]
        return project_path, project_name

    return None, None


def get_cabal_project_dir_of_view(view):
    return locate_cabal_project_from_view(view)[0]


def get_cabal_project_dir_of_file(filename):
    """Return the path to the .cabal file project for the specified file."""
    return locate_cabal_project(filename)[0]


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
    ## Uncomment if instance variables are needed.
    # def __init__(self, view):
    #     super().__init__(view)

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
    return view.substr(view.line(location))


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
    if not name:
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
        return u'QualifiedSymbol(name: \'{0}\', module \'{1}\', '.format(self.name, self.module) + \
               u'module_as \'{0}\', is_import_list \'{1}\', is_operator \'{2}\''.format(self.module_as, self.is_import_list,
                                                                                        self.is_operator)

    def qualified_name(self):
        if self.name is None:
            return self.module
        return u'{0}.{1}'.format(self.module_as or self.module, self.name) if self.module else self.name

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
    def normalize_name(re_result):
        name = next(i for i in [re_result.group('identifier'), re_result.group('operator')] if i is not None)
        return name if name else None

    res = IMPORT_SYMBOL_RE.search(line)
    if res:
        return QualifiedSymbol(name=normalize_name(res),
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
                           name=normalize_name(res),
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


@functools.total_ordering
class ProcessStatusMessage(object):
    counter = itertools.count()

    def __init__(self, msg, timeout=300.0, priority=0):
        self.msg = msg
        self.priority = priority
        self.msg_id = next(self.counter)
        self.timeout = timeout
        self.result = None

    def is_active(self):
        return self.timeout >= 0.0 and self.result is None

    def tick(self, interval):
        self.timeout -= interval
        return self

    def message(self, ticks):
        if self.is_active():
            return u'{0}{1}'.format(self.msg, '.' * (ticks % 4))

        # not self.is_active():
        return u'{0} {1}'.format(self.msg, '[timeout]' if self.result is None else (u'\u2713' if self.result else u'\u2717'))

    def change_message(self, new_msg):
        self.msg = new_msg

    def result_ok(self):
        self.result = True

    def result_fail(self):
        self.result = False

    def __eq__(self, rhs):
        return self.priority == rhs.priority and self.msg_id == rhs.msg_id

    def __gt__(self, rhs):
        # This message belongs farther back in the queue if its message id is smaller than the rhs-compared message
        # id. In othe words, newer (later) messages sort lower.
        return self.priority > rhs.priority or (self.priority == rhs.priority and self.msg_id < rhs.msg_id)


class StatusMessagesManager(threading.Thread):
    # msg ⇒ StatusMessage
    messages = []
    msglock = threading.RLock()

    def __init__(self):
        super().__init__()
        # self.daemon = True
        self.interval = 0.5
        self.event = threading.Event()
        self.ticks = 0
        self.timer = None

    def run(self):
        while True:
            self.event.wait()
            self.event.clear()
            self.ticks = 0
            # Ok, there are some messages, start showing them
            ## print('smgr, enter show: len(self.messages) = {0}'.format(len(self.messages)))
            while self.show():
                self.timer = threading.Timer(self.interval, self.tick)
                self.timer.start()
                self.timer.join()
                ## print('smgr, show: len(self.messages) = {0}'.format(len(self.messages)))


    def show(self):
        msg = ''
        with self.msglock:
            if self.messages:
                msg = self.messages[0].message(self.ticks)

        if msg:
            sublime_status_message(msg)
            return True

        return False


    def tick(self):
        self.ticks = self.ticks + 1
        with self.msglock:
            self.messages = [m for m in self.messages if m.tick(self.interval).is_active()]
        self.show()


    def add(self, new_message):
        with self.msglock:
            heapq.heappush(self.messages, new_message)

        self.event.set()

    def remove(self, msg):
        def do_remove():
            with self.msglock:
                self.messages = [m for m in self.messages if m != msg]
                # Note: heapify does its work in-place.
                heapq.heapify(self.messages)
                self.show()
                self.event.set()

        if msg in self.messages:
            tmo = 4.5
            if len(self.messages) > 1:
                tmo = 0.0
            threading.Timer(tmo, do_remove).start()


STATUS_MSG_MANAGER = StatusMessagesManager()
STATUS_MSG_MANAGER.start()


class StatusMessageContext(object):
    def __init__(self, msg):
        self.msg = msg

    def __enter__(self):
        self.start()
        return self

    def __exit__(self, exc_type, exc_value, exc_traceback):
        if exc_type:
            self.msg.result_fail()
        else:
            self.msg.result_ok()
        self.stop()

    def start(self):
        STATUS_MSG_MANAGER.add(self.msg)

    def stop(self):
        STATUS_MSG_MANAGER.remove(self.msg)


    def change_message(self, new_msg):
        self.msg.change_message(new_msg)

    def result_ok(self):
        self.msg.result_ok()
        STATUS_MSG_MANAGER.show()
        self.stop()

    def result_fail(self):
        self.msg.result_fail()
        STATUS_MSG_MANAGER.show()
        self.stop()


def status_message_process(msg, timeout=300.0, priority=0):
    return StatusMessageContext(ProcessStatusMessage(msg, timeout=timeout, priority=priority))


def settings_has_syntax(settings, syntax):
    # print('settings.get(syntax) = {0}'.format(settings.get('syntax')))
    _, synfile = os.path.split(settings.get('syntax') or '')
    return synfile and synfile.lower() == syntax.lower()


def view_has_syntax(view, syntax):
    if not view:
        view = window_view_and_file(view)[1]
        if not view:
            return False

    return settings_has_syntax(view.settings(), syntax)


def view_is_cabal_source(view):
    return view_has_syntax(view, "Cabal.tmLanguage")


def settings_has_cabal_source(settings):
    return settings_has_syntax(settings, "Cabal.tmLanguage")

HASKELL_SYNTAXES = ["Haskell.tmLanguage",
                    "Haskell.sublime-syntax",
                    'Haskell-SublimeHaskell.tmLanguage']


def view_is_haskell_source(view):
    return any([view_has_syntax(view, syn) for syn in HASKELL_SYNTAXES])


def settings_has_haskell_source(settings):
    return any([settings_has_syntax(settings, syn) for syn in HASKELL_SYNTAXES])


def view_is_inspected_source(view):
    return view_is_haskell_source(view) or view_is_cabal_source(view)


def view_is_haskell_repl(view):
    return any(view_has_syntax(view, syn) for syn in ['HaskellRepl.tmLanguage',
                                                      'HaskellRepl.sublime-syntax'])


def view_is_haskell_symbol_info(view):
    return view_has_syntax(view, "HaskellSymbolInfo.tmLanguage")


def sublime_haskell_cache_path():
    """Get the path where compiled tools and caches are stored"""
    return os.path.join(sublime.cache_path(), 'SublimeHaskell')
