import os
import re
import sublime
import time
from threading import Thread

from sublime_haskell_common import log, are_paths_equal, call_and_wait, get_setting_async

ERROR_PANEL_NAME = 'haskell_error_checker'

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# The first line is divided into a filename, a line number, and a column.
output_regex = re.compile(
    r'^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)',
    re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
result_file_regex = r'^(\S*?): line (\d+), column (\d+):$'

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

    def find_region_in_view(self, view):
        "Return the Region referred to by this error message."
        # Convert line and column count to zero-based indices:
        point = view.text_point(self.line - 1, self.column - 1)
        # Return the whole line:
        region = view.line(point)
        region = trim_region(view, region)
        return region

def run_build_thread(view, cabal_project_dir, msg, cmd):
    run_chain_build_thread(view, cabal_project_dir, msg, [cmd])

def run_chain_build_thread(view, cabal_project_dir, msg, cmds):
    sublime.status_message(msg + '...')
    thread = Thread(
        target=wait_for_chain_to_complete,
        args=(view, cabal_project_dir, msg, cmds))
    thread.start()

def wait_for_build_to_complete(view, cabal_project_dir, msg, cmd):
    """Start 'cabal build', wait for it to complete, then parse and diplay
    the resulting errors."""

    wait_for_chain_to_complete(view, cabal_project_dir, msg, [cmd])

def wait_for_chain_to_complete(view, cabal_project_dir, msg, cmds):
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

    parse_output_messages_and_show(view, msg, cabal_project_dir, exit_code, stderr)

def format_output_messages(messages):
    """Formats list of messages"""
    return u'\n'.join(unicode(x) for x in messages)

def show_output_result_text(view, msg, text, exit_code, base_dir):
    """Shows text (formatted messages) in output with build result"""

    success = exit_code == 0

    success_message = 'SUCCEEDED' if success else 'FAILED'
    output = u'{0}\n\nBuild {1}'.format(text, success_message)

    if success:
        sublime.set_timeout(lambda: sublime.status_message(msg + u" \u2714"), 0)
    else:
        sublime.set_timeout(lambda: sublime.status_message(msg + u" \u2717"), 0)
        if get_setting_async('show_output_window'):
            sublime.set_timeout(lambda: write_output(view, output, base_dir), 0)

def parse_output_messages_and_show(view, msg, base_dir, exit_code, stderr):
    """Parse errors and display resulting errors"""

    # stderr/stdout can contain unicode characters
    stderr = stderr.decode('utf-8')

    # The process has terminated; parse and display the output:
    parsed_messages = parse_output_messages(base_dir, stderr)
    output_text = format_output_messages(parsed_messages) if parsed_messages else stderr

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
            errors_in_view = filter(
                lambda x: are_paths_equal(view_filename, x.filename),
                errors)
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
    edit = output_view.begin_edit()
    #output_view.insert(edit, 0, text)
    output_view.insert(edit, output_view.size(), text)
    output_view.end_edit(edit)
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
