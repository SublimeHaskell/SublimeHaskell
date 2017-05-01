# -*- coding: UTF-8 -*-

import os
import os.path
import re
import time
import threading

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.symbols as symbols

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# It also eats whitespace before the first line.
# The first line is divided into a filename, a line number, and a column.
OUTPUT_REGEX = re.compile(r'\s*^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)', re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
RESULT_FILE_REGEX = r'^\s{2}(\S*?): line (\d+), column (\d+):$'

OUTPUT_PANEL_NAME = 'sublime_haskell_output_panel'

BUILD_LOG_PANEL_NAME = 'sublime_haskell_build_log_panel'

# Global list of errors.
ERRORS = []

# Global ref to view with errors
ERROR_VIEW = None


def filename_of_path(path):
    """Returns everything after the last slash or backslash."""
    # Not using os.path here because we don't know/care here if
    # we have forward or backslashes on Windows.
    return re.match(r'(.*[/\\])?(.*)', path).groups()[1]


class OutputMessage(object):
    "Describe an error or warning message produced by GHC."
    def __init__(self, filename, region, message, level, correction=None):
        self.filename = filename
        self.region = region
        self.message = message
        self.level = level
        self.correction = correction

    def __unicode__(self):
        # must match RESULT_FILE_REGEX
        # TODO: Columns must be recalculated, such that one tab is of tab_size length
        # We can do this for opened views, but how to do this for files, that are not open?
        return u'  {0}: line {1}, column {2}:\n    {3}'.format(
            self.filename,
            self.region.start.line + 1,
            self.region.start.column + 1,
            self.message)

    def __str__(self):
        return self.__unicode__()

    def __repr__(self):
        return '<OutputMessage {0}:{1}-{2}: {3}>'.format(
            filename_of_path(self.filename),
            self.region.start.__repr__(),
            self.region.end.__repr__(),
            self.message[:10] + '..')

    def to_region(self, view):
        "Return the Region referred to by this error message."
        # Convert line and column count to zero-based indices:
        if self.region.empty():
            return trim_region(view, view.line(self.region.start.to_point(view)))
        return self.region.to_region(view)

    def update_region(self):
        self.region.update()
        if self.correction and self.correction.corrector:
            self.correction.corrector.region.update()

    def erase_from_view(self):
        self.region.erase()
        if self.correction and self.correction.corrector:
            self.correction.corrector.region.erase()


def clear_error_marks():
    global ERRORS
    for err in ERRORS:
        err.erase_from_view()
    ERRORS = []


def set_global_error_messages(messages):
    clear_error_marks()
    ERRORS.extend(messages)


def run_build_thread(view, cabal_project_dir, msg, cmd, on_done):
    run_chain_build_thread(view, cabal_project_dir, msg, [cmd], on_done)


def run_chain_build_thread(view, cabal_project_dir, msg, cmds, on_done):
    Common.show_status_message_process(msg, priority=3)
    thread = threading.Thread(target=wait_for_chain_to_complete, args=(view, cabal_project_dir, msg, cmds, on_done))
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
    collected_out = []
    output_log = Common.output_panel(view.window(), '',
                                     panel_name=BUILD_LOG_PANEL_NAME,
                                     panel_display=Settings.PLUGIN.show_output_window)
    for cmd in cmds:
        Common.output_text(output_log, ' '.join(cmd) + '...\n')

        # Don't tie stderr to stdout, since we're interested in the error messages
        out = OutputCollector.OutputCollector(output_log, cmd, cwd=cabal_project_dir)
        exit_code, cmd_out = out.wait()
        collected_out.append(cmd_out)

        # Bail if the command failed...
        if exit_code != 0:
            break

    if len(collected_out) > 0:
        # We're going to show the errors in the output panel...
        Common.hide_panel(view.window(), panel_name=BUILD_LOG_PANEL_NAME)

    # Notify UI thread that commands are done
    sublime.set_timeout(on_done, 0)
    parse_output_messages_and_show(view, msg, cabal_project_dir, exit_code, ''.join(collected_out))


def format_output_messages(messages):
    """Formats list of messages"""
    summary = {'error': 0, 'warning': 0, 'hint': 0}
    for msg in messages:
        summary[msg.level] = summary[msg.level] + 1
    summary_line = 'Errors: {0}, Warnings: {1}, Hints: {2}'.format(summary['error'], summary['warning'], summary['hint'])

    def messages_level(name, level):
        if not summary[level]:
            return ''
        count = '{0}: {1}'.format(name, summary[level])
        msgs = '\n'.join(str(m) for m in messages if m.level == level)
        return '{0}\n\n{1}'.format(count, msgs)

    errors = messages_level('Errors', 'error')
    warnings = messages_level('Warnings', 'warning')
    hints = messages_level('Hints', 'hint')

    return '\n\n'.join(filter(lambda s: s, [summary_line, errors, warnings, hints]))


def show_output_result_text(view, msg, text, exit_code, base_dir):
    """Shows text (formatted messages) in output with build result"""

    success = exit_code == 0

    success_message = 'SUCCEEDED' if success else 'FAILED'
    output = u'Build {0}\n\n{1}'.format(success_message, text.strip())

    Common.show_status_message_process(msg, success)
    # Show panel if there is any text to show (without the part that we add)
    if text and Settings.PLUGIN.show_error_window:
        sublime.set_timeout(lambda: write_output(view, output, base_dir), 0)


def parse_output_messages_and_show(view, msg, base_dir, exit_code, stderr):
    """Parse errors and display resulting errors"""

    # The process has terminated; parse and display the output:
    parsed_messages = parse_output_messages(view, base_dir, stderr)
    # The unparseable part (for other errors)
    unparsable = OUTPUT_REGEX.sub('', stderr).strip()

    # Set global error list
    set_global_error_messages(parsed_messages)

    # If we couldn't parse any messages, just show the stderr
    # Otherwise the parsed errors and the unparsable stderr remainder
    outputs = []

    if parsed_messages:
        outputs += [format_output_messages(parsed_messages)]
        if unparsable:
            outputs += ['', '']
    if unparsable:
        outputs += ["Collected output:\n", unparsable]

    show_output_result_text(view, msg, '\n'.join(outputs), exit_code, base_dir)
    sublime.set_timeout(lambda: mark_messages_in_views(parsed_messages), 0)


def mark_messages_in_views(errors):
    "Mark the regions in open views where errors were found."
    begin_time = time.clock()
    # Mark each diagnostic in each open view in all windows:
    view_filename = ''
    for win in sublime.windows():
        for view in win.views():
            view_filename = view.file_name()
            # Unsaved files have no file name
            if view_filename is None:
                continue
            errors_in_view = list(filter(lambda x: os.path.samefile(view_filename, x.filename), errors))
            mark_messages_in_view(errors_in_view, view)
    end_time = time.clock()
    Logging.log('total time to mark {0} diagnostics: {1} seconds'.format(len(errors), end_time - begin_time),
                Logging.LOG_DEBUG)


MESSAGE_LEVELS = {
    'hint': {
        'style': 'sublimehaskell.mark.hint',
        'icon': {'normal': 'haskell-hint.png',
                 'fix': 'haskell-hint-fix.png'
                }
    },
    'warning': {'style': 'sublimehaskell.mark.warning',
                'icon': {'normal': 'haskell-warning.png',
                         'fix': 'haskell-warning-fix.png'
                        }
               },
    'error': {'style': 'sublimehaskell.mark.error',
              'icon': {'normal': 'haskell-error.png',
                       'fix': 'haskell-error-fix.png'
                      }
             }
}


def errors_for_view(view):
    errs = []
    for err in ERRORS:
        if os.path.samefile(err.filename, view.file_name()):
            err.update_region()
            errs.append(err)
    return sorted(errs, key=lambda e: e.region)


def update_messages_in_view(view, errors):
    mark_messages_in_view(errors, view)


# These next and previous commands were shamelessly copied
# from the great SublimeClang plugin.


def goto_error(view, error):
    line = error.region.start.line + 1
    column = error.region.start.column + 1
    filename = error.filename
    # global ERROR_VIEW
    if ERROR_VIEW:
        show_output(view)
        # error_region = ERROR_VIEW.find('{0}: line {1}, column \\d+:(\\n\\s+.*)*'.format(re.escape(filename), line), 0)
        error_region = ERROR_VIEW.find(re.escape(str(error)), 0)
        # error_region = ERROR_VIEW.find('\\s{{2}}{0}: line {1}, column {2}:(\\n\\s+.*)*'.format(re.escape(filename),
        #                                                                                        line, column), 0)
        ERROR_VIEW.add_regions("current_error", [error_region], 'string', 'dot', sublime.HIDDEN)
        ERROR_VIEW.show(error_region.a)
    view.window().open_file("{0}:{1}:{2}".format(filename, line, column), sublime.ENCODED_POSITION)


class SublimeHaskellNextError(Common.SublimeHaskellTextCommand):
    def __init__(self, view):
        super().__init__(view)

    def run(self, _edit):
        errs = errors_for_view(self.view)
        if not errs:
            Common.show_status_message('No errors or warnings!', priority=5)
        else:
            view_pt = self.view.sel()[0]
            # Bump just past the view's point, just in case we're sitting on top of the current
            cur_point = symbols.Region.from_region(self.view, view_pt)
            err_iter = filter(lambda e: e.region > cur_point, errs)
            next_err = next(err_iter, None)
            # If the view's point is really on top of the start of an error, move to the next, otherwise,
            # we'll just keep sitting on top of the current error and never move.
            if next_err is not None and next_err.region.start == cur_point.start:
                next_err = next(err_iter, None)
            # Cycle around to the first error if we run off the end of the list.
            if next_err is None:
                next_err = errs[0]
            self.view.sel().clear()
            self.view.sel().add(next_err.region.to_region(self.view))
            goto_error(self.view, next_err)


class SublimeHaskellPreviousError(Common.SublimeHaskellTextCommand):
    def __init__(self, view):
        super().__init__(view)

    def run(self, _edit):
        errs = errors_for_view(self.view)
        if not errs:
            Common.show_status_message("No errors or warnings!", priority=5)
        else:
            cur_point = symbols.Region.from_region(self.view, self.view.sel()[0])
            prev_err = next(filter(lambda e: e.region < cur_point, reversed(errs)), None)
            # Cycle around to the last error if we run off the first
            if prev_err is None:
                prev_err = errs[-1]
            self.view.sel().clear()
            self.view.sel().add(prev_err.region.to_region(self.view))
            goto_error(self.view, prev_err)


def region_key(name, is_fix=False):
    if is_fix:
        return 'output-{0}s-fix'.format(name)
    else:
        return 'output-{0}s'.format(name)


def get_icon(png):
    return "/".join([
        "Packages",
        os.path.basename(os.path.dirname(__file__)),
        "Icons",
        png])


def mark_messages_in_view(messages, view):
    for msg in messages:
        msg.erase_from_view()

    for i, msg in enumerate(messages):
        msg.region.save(view, '{0}-{1}'.format(region_key(msg.level, msg.correction is not None), str(i)))
        view.add_regions(msg.region.region_key,
                         [msg.to_region(view)],
                         MESSAGE_LEVELS[msg.level]['style'],
                         get_icon(MESSAGE_LEVELS[msg.level]['icon']['fix' if msg.correction is not None else 'normal']),
                         sublime.DRAW_OUTLINED)
        if msg.correction and msg.correction.corrector:
            msg.correction.corrector.region.save(view, 'autofix-{0}'.format(str(i)))
            view.add_regions(msg.correction.corrector.region.region_key,
                             [msg.correction.corrector.region.to_region(view)],
                             'autofix.region',
                             '',
                             sublime.HIDDEN)


def write_output(view, text, cabal_project_dir, panel_out=True):
    "Write text to Sublime's output panel."
    global ERROR_VIEW
    ERROR_VIEW = Common.output_panel(view.window(), text,
                                     panel_name=OUTPUT_PANEL_NAME,
                                     syntax='HaskellOutputPanel',
                                     panel_display=panel_out)
    ERROR_VIEW.settings().set("result_file_regex", RESULT_FILE_REGEX)
    ERROR_VIEW.settings().set("result_base_dir", cabal_project_dir)


def hide_output(view, panel_name=OUTPUT_PANEL_NAME):
    view.window().run_command('hide_panel', {'panel': 'output.' + panel_name})


def show_output(view, panel_name=OUTPUT_PANEL_NAME):
    ## view.set_read_only(True)
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
    for char in cur_line:
        if col >= column:
            return real_col
        col += (8 if char == '\t' else 1)
        real_col += 1
    return real_col


def parse_output_messages(view, base_dir, text):
    "Parse text into a list of OutputMessage objects."
    matches = OUTPUT_REGEX.finditer(text)

    def to_error(errmsg):
        filename, line, column, messy_details = errmsg.groups()
        line, column = int(line), int(column)

        column = ghc_column_to_sublime_column(view, line, column)
        line = line - 1
        # Record the absolute, normalized path.
        return OutputMessage(os.path.normpath(os.path.join(base_dir, filename)),
                             symbols.Region(symbols.Position(line, column)),
                             messy_details.strip(),
                             'warning' if 'warning' in messy_details.lower() else 'error')

    return list(map(to_error, matches))


def trim_region(view, region):
    "Return the specified Region, but without leading or trailing whitespace."
    text = view.substr(region)
    # Regions may be selected backwards, so b could be less than a.
    rgn_begin = region.begin()
    rgn_end = region.end()
    # Figure out how much to move the endpoints to lose the space.
    # If the region is entirely whitespace, give up and return it unchanged.
    if text.isspace():
        return region
    else:
        text_trimmed_on_left = text.lstrip()
        text_trimmed = text_trimmed_on_left.rstrip()
        rgn_begin += len(text) - len(text_trimmed_on_left)
        rgn_end -= len(text_trimmed_on_left) - len(text_trimmed)
        return sublime.Region(rgn_begin, rgn_end)

DATA_REGEX = re.compile(r'(?P<what>(newtype|type|data))\s+((?P<ctx>(.*))=>\s+)?(?P<name>\S+)\s+' + \
                        r'(?P<args>(\w+\s+)*)=(\s*(?P<def>.*)\s+-- Defined)?',
                        re.MULTILINE)
CLASS_REGEX = re.compile(r'(?P<what>class)\s+((?P<ctx>(.*))=>\s+)?(?P<name>\S+)\s+(?P<args>(\w+\s+)*)(.*)where$',
                         re.MULTILINE)

def parse_info(name, contents):
    """
    Parses result of :i <name> command of ghci and returns derived symbols.Declaration
    """
    if name[0].isupper():
        # data, class, type or newtype
        matched = DATA_REGEX.search(contents) or CLASS_REGEX.search(contents)
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
        function_regex = r'{0}\s+::\s+(?P<type>.*?)(\s+--(.*))?$'.format(name)
        matched = re.search(function_regex, contents, re.MULTILINE)
        if matched:
            return symbols.Function(name, matched.group('type'))

    return None
