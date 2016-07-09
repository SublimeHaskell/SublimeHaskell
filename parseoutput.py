# -*- coding: UTF-8 -*-

import os
import os.path
import re
import sublime
import time
from sys import version
from threading import Thread
from collections import defaultdict

PyV3 = version[0] == "3"

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import symbols
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.symbols as symbols

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# It also eats whitespace before the first line.
# The first line is divided into a filename, a line number, and a column.
output_regex = re.compile(
    r'\s*^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)',
    re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
result_file_regex = r'^(\S*?): line (\d+), column (\d+):$'

OUTPUT_PANEL_NAME = 'sublime_haskell_output_panel'

BUILD_LOG_PANEL_NAME = 'sublime_haskell_build_log_panel'

# Global list of errors. Used e.g. for jumping to the next one.
# Properly assigned being a defaultdict in clear_error_marks().
# Structure: ERRORS[filename][m.start.line] = OutputMessage()
ERRORS = {}

# Global ref to view with errors
error_view = None


def filename_of_path(p):
    """Returns everything after the last slash or backslash."""
    # Not using os.path here because we don't know/care here if
    # we have forward or backslashes on Windows.
    return re.match(r'(.*[/\\])?(.*)', p).groups()[1]


class OutputPoint(object):
    def __init__(self, line, column):
        self.line = int(line)
        self.column = int(column)

    def __unicode__(self):
        return u"{0}:{1}".format(self.line, self.column)

    def __str__(self):
        return self.__unicode__()

    def __eq__(self, other):
        return self.line == other.line and self.column == other.column

    def to_point_of_view(self, view):
        return view.text_point(self.line, self.column)


class OutputMessage(object):
    "Describe an error or warning message produced by GHC."
    def __init__(self, filename, start, end, message, level):
        self.filename = filename
        self.start = start
        self.end = end
        self.message = message.replace(os.linesep, "\n")
        self.level = level

    def __unicode__(self):
        # must match result_file_regex
        # TODO: Columns must be recalculated, such that one tab is of tab_size length
        # We can do this for opened views, but how to do this for files, that are not open?
        return u'{0}: line {1}, column {2}:\n  {3}'.format(
            self.filename,
            self.start.line + 1,
            self.start.column + 1,
            self.message)

    def __str__(self):
        return self.__unicode__()

    def __repr__(self):
        return '<OutputMessage {0}:{1}-{2}: {3}>'.format(
            filename_of_path(self.filename),
            self.start.__repr__(),
            self.end.__repr__(),
            self.message[:10] + '..')

    def to_region_in_view(self, view):
        "Return the Region referred to by this error message."
        # Convert line and column count to zero-based indices:
        if self.start == self.end:  # trimmed full line
            return trim_region(view, view.line(self.start.to_point_of_view(view)))
        return sublime.Region(self.start.to_point_of_view(view), self.end.to_point_of_view(view))


def clear_error_marks():
    global ERRORS

    def listdict():
        return defaultdict(list)
    # listdict = lambda: defaultdict(list)
    ERRORS = defaultdict(listdict)


def set_global_error_messages(messages):
    global ERRORS

    clear_error_marks()

    for m in messages:
        ERRORS[m.filename][m.start.line].append(m)


def run_build_thread(view, cabal_project_dir, msg, cmd, on_done):
    run_chain_build_thread(view, cabal_project_dir, msg, [cmd], on_done)


def run_chain_build_thread(view, cabal_project_dir, msg, cmds, on_done):
    show_status_message_process(msg, priority = 3)
    thread = Thread(
        target=wait_for_chain_to_complete,
        args=(view, cabal_project_dir, msg, cmds, on_done))
    thread.start()


def wait_for_build_to_complete(view, cabal_project_dir, msg, cmd, on_done):
    """Run a command, wait for it to complete, then parse and display
    the resulting errors."""

    wait_for_chain_to_complete(view, cabal_project_dir, msg, [cmd], on_done)


def wait_for_chain_to_complete(view, cabal_project_dir, msg, cmds, on_done):
    """Chains several commands, wait for them to complete, then parse and display
    the resulting errors."""

    # First hide error panel to show that something is going on
    sublime.set_timeout(lambda: hide_output(view), 0)

    # run and wait commands, fail on first fail
    # stdout = ''
    stderr = ''
    output_log = output_panel(view.window(), '', panel_name = BUILD_LOG_PANEL_NAME, show_panel = get_setting_async('show_output_window'))
    for cmd in cmds:
        output_text(output_log, ' '.join(cmd) + '...\n')

        cmd_p = call_and_wait(cmd, cwd = cabal_project_dir, wait = False)
        lines = []
        for cmd_line in cmd_p.stdout:
            line = crlf2lf(decode_bytes(cmd_line))
            lines.append(line)
            output_text(output_log, line)
            output_log.show(output_log.size())  # Scroll to the end
        exit_code = cmd_p.wait()
        # stdout = '\n'.join(lines)
        stderr = crlf2lf(decode_bytes(cmd_p.stderr.read()))
    hide_panel(view.window(), panel_name = BUILD_LOG_PANEL_NAME)

    errmsg = stderr

    # Notify UI thread that commands are done
    sublime.set_timeout(on_done, 0)

    parse_output_messages_and_show(view, msg, cabal_project_dir, exit_code, errmsg)


def format_output_messages(messages):
    """Formats list of messages"""
    summary = {'error': 0, 'warning': 0, 'hint': 0}
    for m in messages:
        summary[m.level] = summary[m.level] + 1
    summary_line = 'Errors: {0}, Warnings: {1}, Hints: {2}'.format(
        summary['error'],
        summary['warning'],
        summary['hint'])
    if PyV3:
        details = '\n'.join(str(x) for x in messages)
        return '{0}\n\n{1}'.format(summary_line, details) if details else ''
    else:
        details = u'\n'.join(unicode(x) for x in messages)
        return u'{0}\n\n{1}'.format(summary_line, details) if details else u''


def show_output_result_text(view, msg, text, exit_code, base_dir):
    """Shows text (formatted messages) in output with build result"""

    success = exit_code == 0

    success_message = 'SUCCEEDED' if success else 'FAILED'
    output = u'Build {0}\n\n{1}'.format(success_message, text.strip())

    show_status_message_process(msg, success)
    # Show panel if there is any text to show (without the part that we add)
    if text:
        if get_setting_async('show_error_window'):
            sublime.set_timeout(lambda: write_output(view, output, base_dir), 0)


def parse_output_messages_and_show(view, msg, base_dir, exit_code, stderr):
    """Parse errors and display resulting errors"""

    # stderr/stdout can contain unicode characters
    # already done in call_and_wait
    # stderr = stderr.decode('utf-8')

    # The process has terminated; parse and display the output:
    parsed_messages = parse_output_messages(view, base_dir, stderr)
    # The unparseable part (for other errors)
    unparsable = output_regex.sub('', stderr).strip()

    # Set global error list
    set_global_error_messages(parsed_messages)

    # If we couldn't parse any messages, just show the stderr
    # Otherwise the parsed errors and the unparsable stderr remainder
    outputs = []

    if parsed_messages:
        outputs += [format_output_messages(parsed_messages)]
    if unparsable:
        outputs += ["\nREMAINING STDERR:\n", unparsable]

    output_text = '\n'.join(outputs)

    show_output_result_text(view, msg, output_text, exit_code, base_dir)

    sublime.set_timeout(lambda: mark_messages_in_views(parsed_messages), 0)


def mark_messages_in_views(errors):
    "Mark the regions in open views where errors were found."
    begin_time = time.clock()
    # Mark each diagnostic in each open view in all windows:
    for w in sublime.windows():
        for v in w.views():
            view_filename = v.file_name()
            # Unsaved files have no file name
            if view_filename is None:
                continue
            errors_in_view = list(filter(
                lambda x: os.path.samefile(view_filename, x.filename),
                errors))
            mark_messages_in_view(errors_in_view, v)
    end_time = time.clock()
    log('total time to mark {0} diagnostics: {1} seconds'.format(
        len(errors), end_time - begin_time), log_debug)

message_levels = {
    'hint': {
        'style': 'sublimehaskell.mark.hint',
        'icon': 'haskell-hint.png'
    },
    'warning': {
        'style': 'sublimehaskell.mark.warning',
        'icon': 'haskell-warning.png'
    },
    'error': {
        'style': 'sublimehaskell.mark.error',
        'icon': 'haskell-error.png'
    }
}


# These next and previous commands were shamelessly copied
# from the great SublimeClang plugin.

def goto_error(view, filename, line, column):
    global error_view
    if error_view:
        show_output(view)
        # error_region = error_view.find('{0}: line {1}, column \\d+:(\\n\\s+.*)*'.format(re.escape(filename), line), 0)
        error_region = error_view.find('{0}: line {1}, column {2}:(\\n\\s+.*)*'.format(re.escape(filename), line, column), 0)
        error_view.add_regions("current_error", [error_region], 'string', 'dot', sublime.HIDDEN)
        error_view.show(error_region.a)
    view.window().open_file("{0}:{1}:{2}".format(filename, line, column), sublime.ENCODED_POSITION)


def get_next_value(v, lst, cycle = True):
    # Get next value from list
    if v is None:
        if len(lst) > 0:
            return lst[0]
        return None
    for x in filter(lambda k: k > v, lst):
        return x
    if cycle and len(lst) > 0:
        return lst[0]
    return None


class SublimeHaskellNextError(SublimeHaskellTextCommand):
    def run(self, edit):
        v = self.view
        fn = v.file_name()
        line, column = v.rowcol(v.sel()[0].a)
        # line += 1
        gotoline = None
        gotocolumn = None
        if fn in ERRORS:
            if line in ERRORS[fn]:  # on some line, check if there are another error on same linee
                gotoline = line
                gotocolumn = get_next_value(column, sorted([e.start.column for e in ERRORS[fn][gotoline]]), cycle = False)
                if gotocolumn is not None:  # next error on same line
                    goto_error(v, fn, gotoline + 1, gotocolumn + 1)
                    return
            # no error on this line, find next (and cycle through)
            gotoline = get_next_value(line, sorted(ERRORS[fn].keys()))
            if gotoline is not None:
                # go to first error on line
                gotocolumn = get_next_value(None, sorted([e.start.column for e in ERRORS[fn][gotoline]]), cycle = False)
                if gotocolumn is not None:  # found some
                    goto_error(v, fn, gotoline + 1, gotocolumn + 1)
                    return
            show_status_message('No more errors or warnings!', priority = 5)


class SublimeHaskellPreviousError(SublimeHaskellTextCommand):
    def run(self, edit):
        v = self.view
        fn = v.file_name()
        line, column = v.rowcol(v.sel()[0].a)
        line += 1
        gotoline = -1
        if fn in ERRORS:
            for errLine in sorted(ERRORS[fn].keys(), key = lambda x: -x):
                if errLine < line:
                    gotoline = errLine
                    break
            # No previous line: Wrap around if possible
            if gotoline == -1 and len(ERRORS[fn]) > 0:
                gotoline = sorted(ERRORS[fn].keys())[-1]
        if gotoline != -1:
            goto_error(v, fn, gotoline)
        else:
            sublime.status_message("No more errors or warnings!")


def region_key(name):
    return 'subhs-{0}s'.format(name)


def get_icon(png):
    return "/".join([
        "Packages",
        os.path.basename(os.path.dirname(__file__)),
        "Icons",
        png])


def mark_messages_in_view(messages, view):
    # Regions by level
    regions = {}
    for k in message_levels.keys():
        regions[k] = []

    for m in messages:
        regions[m.level].append(m.to_region_in_view(view))

    for nm, lev in message_levels.items():
        view.erase_regions(region_key(nm))
        view.add_regions(
            region_key(nm),
            regions[nm],
            lev['style'],
            get_icon(lev['icon']),
            sublime.DRAW_OUTLINED)


def write_output(view, text, cabal_project_dir, show_panel = True):
    "Write text to Sublime's output panel."
    global error_view
    error_view = output_panel(view.window(), text, panel_name = OUTPUT_PANEL_NAME, syntax = 'HaskellOutputPanel', show_panel = show_panel)
    error_view.settings().set("result_file_regex", result_file_regex)
    error_view.settings().set("result_base_dir", cabal_project_dir)


def hide_output(view, panel_name = OUTPUT_PANEL_NAME):
    view.window().run_command('hide_panel', {'panel': 'output.' + panel_name})


def show_output(view, panel_name = OUTPUT_PANEL_NAME):
    view.window().run_command('show_panel', {'panel': 'output.' + panel_name})


def tabs_offset(view, point):
    """
    Returns count of '\t' before point in line multiplied by 7
    8 is size of type as supposed by ghc-mod, to every '\t' will add 7 to column
    Subtract this value to get sublime column by ghc-mod column, add to get ghc-mod column by sublime column
    """
    cur_line = view.substr(view.line(point))
    return len(list(filter(lambda ch: ch == '\t', cur_line))) * 7


def sublime_column_to_ghc_column(view, line, column):
    """
    Convert sublime zero-based column to ghc-mod column (where tab is 8 length)
    """
    return column + tabs_offset(view, view.text_point(line, column)) + 1


def ghc_column_to_sublime_column(view, line, column):
    """
    Convert ghc-mod column to sublime zero-based column
    """
    cur_line = view.substr(view.line(view.text_point(line - 1, 0)))
    col = 1
    real_col = 0
    for c in cur_line:
        if col >= column:
            return real_col
        col += (8 if c == '\t' else 1)
        real_col += 1
    return real_col


def parse_output_messages(view, base_dir, text):
    "Parse text into a list of OutputMessage objects."
    matches = output_regex.finditer(text)

    def to_error(m):
        filename, line, column, messy_details = m.groups()
        line, column = int(line), int(column)

        column = ghc_column_to_sublime_column(view, line, column)
        line = line - 1
        return OutputMessage(
            # Record the absolute, normalized path.
            os.path.normpath(os.path.join(base_dir, filename)),
            OutputPoint(line, column),
            OutputPoint(line, column),
            messy_details.strip(),
            'warning' if 'warning' in messy_details.lower() else 'error')

    return list(map(to_error, matches))


def trim_region(view, region):
    "Return the specified Region, but without leading or trailing whitespace."
    text = view.substr(region)
    # Regions may be selected backwards, so b could be less than a.
    a = min(region.a, region.b)
    b = max(region.a, region.b)
    # Figure out how much to move the endpoints to lose the space.
    # If the region is entirely whitespace, give up and return it unchanged.
    if text.isspace():
        return region
    else:
        text_trimmed_on_left = text.lstrip()
        text_trimmed = text_trimmed_on_left.rstrip()
        a += len(text) - len(text_trimmed_on_left)
        b -= len(text_trimmed_on_left) - len(text_trimmed)
        return sublime.Region(a, b)


def parse_info(name, contents):
    """
    Parses result of :i <name> command of ghci and returns derived symbols.Declaration
    """
    functionRegex = '{0}\s+::\s+(?P<type>.*?)(\s+--(.*))?$'.format(name)
    dataRegex = '(?P<what>(newtype|type|data))\s+((?P<ctx>(.*))=>\s+)?(?P<name>\S+)\s+(?P<args>(\w+\s+)*)=(\s*(?P<def>.*)\s+-- Defined)?'
    classRegex = '(?P<what>class)\s+((?P<ctx>(.*))=>\s+)?(?P<name>\S+)\s+(?P<args>(\w+\s+)*)(.*)where$'

    if name[0].isupper():
        # data, class, type or newtype
        matched = re.search(dataRegex, contents, re.MULTILINE) or re.search(classRegex, contents, re.MULTILINE)
        if matched:
            what = matched.group('what')
            args = matched.group('args').strip().split(' ') if matched.group('args') else []
            ctx = matched.group('ctx')
            definition = matched.group('def')
            if definition:
                definition.strip()

            if what == 'class':
                return symbols.Class(name, ctx, args)
            elif what == 'data':
                return symbols.Data(name, ctx, args, definition)
            elif what == 'type':
                return symbols.Type(name, ctx, args, definition)
            elif what == 'newtype':
                return symbols.Newtype(name, ctx, args, definition)
            else:
                raise RuntimeError('Unknown type of symbol: {0}'.format(what))

    else:
        # function
        matched = re.search(functionRegex, contents, re.MULTILINE)
        if matched:
            return symbols.Function(name, matched.group('type'))

    return None
