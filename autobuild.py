import fnmatch
import os
import re
import sublime
import sublime_plugin
import subprocess
from threading import Thread
import time

from sublime_haskell_common import get_cabal_project_dir_of_view, call_and_wait, log, are_paths_equal

ERROR_PANEL_NAME = 'haskell_error_checker'

# Global mappings { view_id -> { lineno -> error } }, lineno is 0-indexed
# (inspired by SublimeLinter)
ERRORS = {}
WARNINGS = {}

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# The first line is divided into a filename, a line number, and a column.
error_output_regex = re.compile(
    r'^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)',
    re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
result_file_regex = r'^(\S*?): line (\d+), column (\d+):$'


# From SublineLinter
def last_selected_lineno(view):
    viewSel = view.sel()
    if not viewSel:
        return None
    return view.rowcol(viewSel[0].end())[0]


class SublimeHaskellAutobuild(sublime_plugin.EventListener):

    def __init__(self):
        super(SublimeHaskellAutobuild, self).__init__()
        self.last_selected_lineno = None

    def on_post_save(self, view):
        # If the edited file was Haskell code within a cabal project, try to
        # compile it.
        cabal_project_dir = get_cabal_project_dir_of_view(view)
        if cabal_project_dir is not None:
            # On another thread, wait for the build to finish.
            sublime.status_message('Rebuilding Haskell...')
            thread = Thread(
                target=wait_for_build_to_complete,
                args=(view, cabal_project_dir))
            thread.start()

    def on_selection_modified(self, view):
        cabal_project_dir = get_cabal_project_dir_of_view(view)

        if cabal_project_dir is not None:
            lineno = last_selected_lineno(view)
            if lineno != self.last_selected_lineno:
                self.last_selected_lineno = lineno

                vid = view.id()

                # Only deal with warnings if there are no errors
                if not ERRORS.get(vid):
                    # We would like to close the panel here when moving away from a warning region,
                    #     hide_output(view)
                    # but does not seem to work as it closes *the active panel* (e.g. also find).
                    # So the user has to close it manually with ESC for now.
                    warning = WARNINGS.get(vid, {}).get(lineno)
                    if warning:
                        write_output(view, unicode(warning), cabal_project_dir)


class ErrorMessage(object):
    "Describe an error or warning message produced by GHC."
    def __init__(self, filename, line, column, message):
        self.filename = filename
        self.line = int(line)
        self.column = int(column)
        self.message = message
        self.is_warning = 'warning' in message.lower()

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

def wait_for_build_to_complete(view, cabal_project_dir):
    """Start 'cabal build', wait for it to complete, then parse and diplay
    the resulting errors."""

    # First hide error panel to show that something is going on
    sublime.set_timeout(lambda: hide_output(view), 0)

    exit_code, stdout, stderr = call_and_wait(
        ['cabal', 'build'],
        cwd=cabal_project_dir)

    # stderr/stdout can contain unicode characters
    stdout = stderr.decode('utf-8')
    stderr = stderr.decode('utf-8')

    success = exit_code == 0

    # The process has terminated; parse and display the output:
    parsed_messages = parse_error_messages(cabal_project_dir, stderr)
    if parsed_messages:
        error_messages = u'\n'.join(unicode(x) for x in parsed_messages)
    else:
        error_messages = stderr
    success_message = 'SUCCEEDED' if success else 'FAILED'
    output = u'{0}\n\nBuild {1}'.format(error_messages, success_message)
    # Use set_timeout() so that the call occurs on the main Sublime thread:
    mark_errors_cb = lambda: mark_errors_in_views(parsed_messages)
    sublime.set_timeout(mark_errors_cb, 0)

    # TODO make this an option
    if success:
        sublime.status_message(u"Rebuilding Haskell \u2714")
    else:
        write_output_cb = lambda: write_output(view, output, cabal_project_dir)
        sublime.set_timeout(write_output_cb, 0)


def mark_errors_in_views(errors):
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
            mark_errors_in_this_view(errors_in_view, v)
    end_time = time.clock()
    log('total time to mark {0} diagnostics: {1} seconds'.format(
        len(errors), end_time - begin_time))

def mark_errors_in_this_view(errors, view):
    WARNING_REGION_KEY = 'subhs-warnings'
    ERROR_REGION_KEY = 'subhs-errors'
    # Clear old regions:
    view.erase_regions(WARNING_REGION_KEY)
    view.erase_regions(ERROR_REGION_KEY)
    # Add all error and warning regions in this view.
    error_regions = []
    warning_regions = []

    # Update global error/warning dicts
    # TODO this is redundant with the other error lists
    vid = view.id()
    # ERRORS, WARNINGS store lines 0-indexed
    ERRORS[vid] = dict( (e.line - 1, e) for e in errors if not e.is_warning )
    WARNINGS[vid] = dict( (e.line - 1, e) for e in errors if e.is_warning )

    for e in errors:
        region = e.find_region_in_view(view)
        if (e.is_warning):
            warning_regions.append(region)
        else:
            error_regions.append(region)
    # Mark warnings:
    view.add_regions(
        WARNING_REGION_KEY,
        warning_regions,
        'comment.warning',
        'grey_x_light_shadow',
        sublime.DRAW_OUTLINED)
    # Mark errors:
    view.add_regions(
        ERROR_REGION_KEY,
        error_regions,
        'invalid',
        'grey_x',
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
    output_view.insert(edit, 0, text)
    output_view.end_edit(edit)
    # Set the selection to the beginning of the view so that "next result" works:
    output_view.sel().clear()
    output_view.sel().add(sublime.Region(0))
    output_view.set_read_only(True)
    # Show the results panel:
    view.window().run_command('show_panel', {'panel': 'output.' + ERROR_PANEL_NAME})
    print "  show view", view
    # view.window().run_command('show_panel', {'panel': 'output.gaga'})

def hide_output(view):
    # TODO The 'panel' key doesn't appear in the API; most probably, it is ignored,
    #      and simply the current panel is hidden.
    # In theory, this should work:
    #     run_command('show_panel', {'panel': 'output.' + ERROR_PANEL_NAME, 'toggle': True})
    # but 'toggle' doesn't seem to work on output panels (it works on the 'console' panel).
    # There doesn't currently seem to be a way to close a panel by name.
    view.window().run_command('hide_panel', {'panel': 'output.' + ERROR_PANEL_NAME})

def parse_error_messages(base_dir, text):
    "Parse text into a list of ErrorMessage objects."
    matches = error_output_regex.finditer(text)

    def to_error(m):
        filename, line, column, messy_details = m.groups()
        return ErrorMessage(
            # Record the absolute, normalized path.
            os.path.normpath(os.path.join(base_dir, filename)),
            line,
            column,
            messy_details.strip())

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
