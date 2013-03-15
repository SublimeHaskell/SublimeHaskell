import os
import re
import sublime
import sublime_plugin
import time
from threading import Thread
from collections import defaultdict

if int(sublime.version()) < 3000:
    from sublime_haskell_common import log, are_paths_equal, call_and_wait, get_setting_async, show_status_message_process, show_status_message
else:
    from SublimeHaskell.sublime_haskell_common import log, are_paths_equal, call_and_wait, get_setting_async, show_status_message_process, show_status_message

ERROR_PANEL_NAME = 'haskell_error_checker'

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# It also eats whitespace before the first line.
# The first line is divided into a filename, a line number, and a column.
output_regex = re.compile(
    r'\s*^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)',
    re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
result_file_regex = r'^(\S*?): line (\d+), column (\d+):$'


# Global list of errors. Used e.g. for jumping to the next one.
# Properly assigned being a defaultdict in clear_error_marks().
# Structure: ERRORS[filename][m.line] = OutputMessage()
ERRORS = {}


def filename_of_path(p):
    """Returns everything after the last slash or backslash."""
    # Not using os.path here because we don't know/care here if
    # we have forward or backslashes on Windows.
    return re.match(r'(.*[/\\])?(.*)', p).groups()[1]


class OutputMessage(object):
    "Describe an error or warning message produced by GHC."
    def __init__(self, filename, line, column, message, level):
        self.filename = filename
        self.line = int(line)
        self.column = int(column)
        self.message = message
        self.level = level

    def __unicode__(self):
        # must match result_file_regex
        return u'{0}: line {1}, column {2}:\n  {3}'.format(
            self.filename,
            self.line,
            self.column,
            self.message)

    def __repr__(self):
        return '<OutputMessage {0}:{1}:{2}: {3}>'.format(
            filename_of_path(self.filename),
            self.line,
            self.column,
            self.message[:10] + '..')

    def find_region_in_view(self, view):
        "Return the Region referred to by this error message."
        # Convert line and column count to zero-based indices:
        point = view.text_point(self.line - 1, self.column - 1)
        # Return the whole line:
        region = view.line(point)
        region = trim_region(view, region)
        return region


def clear_error_marks():
    global ERRORS

    listdict = lambda: defaultdict(list)
    ERRORS = defaultdict(listdict)


def set_global_error_messages(messages):
    global ERRORS

    clear_error_marks()

    for m in messages:
        ERRORS[m.filename][m.line].append(m)


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
    for cmd in cmds:
        exit_code, stdout, stderr = call_and_wait(
            cmd,
            cwd=cabal_project_dir)
        if exit_code != 0:
            break

    # Notify UI thread that commands are done
    sublime.set_timeout(on_done, 0)

    parse_output_messages_and_show(view, msg, cabal_project_dir, exit_code, stderr)


def format_output_messages(messages):
    """Formats list of messages"""
    return u'\n'.join(unicode(x) for x in messages)


def show_output_result_text(view, msg, text, exit_code, base_dir):
    """Shows text (formatted messages) in output with build result"""

    success = exit_code == 0

    success_message = 'SUCCEEDED' if success else 'FAILED'
    output = u'{0}\n\nBuild {1}'.format(text, success_message)

    show_status_message_process(msg, success)
    if not success:
        if get_setting_async('show_output_window'):
            sublime.set_timeout(lambda: write_output(view, output, base_dir), 0)


def parse_output_messages_and_show(view, msg, base_dir, exit_code, stderr):
    """Parse errors and display resulting errors"""

    # stderr/stdout can contain unicode characters
    stderr = stderr.decode('utf-8')

    # The process has terminated; parse and display the output:
    parsed_messages = parse_output_messages(base_dir, stderr)
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
                lambda x: are_paths_equal(view_filename, x.filename),
                errors))
            mark_messages_in_view(errors_in_view, v)
    end_time = time.clock()
    log('total time to mark {0} diagnostics: {1} seconds'.format(
        len(errors), end_time - begin_time))

message_levels = {
    'hint': {
        'style': 'comment.warning',
        'icon': 'light_x_bright'
    },
    'warning': {
        'style': 'comment.warning',
        'icon': 'grey_x_light_shadow'
    },
    'error': {
        'style': 'invalid',
        'icon': 'grey_x'
    }
}


# These next and previous commands were shamelessly copied
# from the great SublimeClang plugin.

class SublimeHaskellNextError(sublime_plugin.TextCommand):
    def run(self, edit):
        log("SublimeHaskellNextError")
        v = self.view
        fn = v.file_name().encode("utf-8")
        line, column = v.rowcol(v.sel()[0].a)
        gotoline = -1
        if fn in ERRORS:
            for errLine in ERRORS[fn]:
                if errLine > line:
                    gotoline = errLine
                    break
            # No next line: Wrap around if possible
            if gotoline == -1 and len(ERRORS[fn]) > 0:
                gotoline = ERRORS[fn].keys()[0]
        if gotoline != -1:
            v.window().open_file("%s:%d" % (fn, gotoline), sublime.ENCODED_POSITION)
        else:
            sublime.status_message("No more errors or warnings!")


class SublimeHaskellPreviousError(sublime_plugin.TextCommand):
    def run(self, edit):
        v = self.view
        fn = v.file_name().encode("utf-8")
        line, column = v.rowcol(v.sel()[0].a)
        gotoline = -1
        if fn in ERRORS:
            for errLine in ERRORS[fn]:
                if errLine < line:
                    gotoline = errLine
            # No previous line: Wrap around if possible
            if gotoline == -1 and len(ERRORS[fn]) > 0:
                gotoline = ERRORS[fn].keys()[-1]
        if gotoline != -1:
            v.window().open_file("%s:%d" % (fn, gotoline), sublime.ENCODED_POSITION)
        else:
            sublime.status_message("No more errors or warnings!")



def region_key(name):
    return 'subhs-{0}s'.format(name)


def mark_messages_in_view(messages, view):
    # Regions by level
    regions = {}
    for k in message_levels.keys():
        regions[k] = []

    for m in messages:
        regions[m.level].append(m.find_region_in_view(view))

    for nm, lev in message_levels.items():
        view.erase_regions(region_key(nm))
        view.add_regions(
            region_key(nm),
            regions[nm],
            lev['style'],
            lev['icon'],
            sublime.DRAW_OUTLINED)


def write_output(view, text, cabal_project_dir):
    "Write text to Sublime's output panel."
    output_view = view.window().get_output_panel(ERROR_PANEL_NAME)
    output_view.set_read_only(False)
    # Configure Sublime's error message parsing:
    output_view.settings().set("result_file_regex", result_file_regex)
    output_view.settings().set("result_base_dir", cabal_project_dir)
    # Write to the output buffer:
    output_view.run_command('sublime_haskell_output_text', {
        'text': text })
    # Set the selection to the beginning of the view so that "next result" works:
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0))
    output_view.set_read_only(True)
    # Show the results panel:
    view.window().run_command('show_panel', {'panel': 'output.' + ERROR_PANEL_NAME})


def hide_output(view):
    view.window().run_command('hide_panel', {'panel': 'output.' + ERROR_PANEL_NAME})


def parse_output_messages(base_dir, text):
    "Parse text into a list of OutputMessage objects."
    matches = output_regex.finditer(text)

    def to_error(m):
        filename, line, column, messy_details = m.groups()
        return OutputMessage(
            # Record the absolute, normalized path.
            os.path.normpath(os.path.join(base_dir, filename)),
            line,
            column,
            messy_details.strip(),
            'warning' if 'warning' in messy_details.lower() else 'error')

    return map(to_error, matches)


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
