# -*- coding: UTF-8 -*-

import os
import os.path
import re
import sublime
import time
from threading import Thread
from collections import defaultdict

if int(sublime.version()) < 3000:
    import sublime_haskell_common as Common
    import internals.logging as Logging
    import internals.settings as Settings
    from internals.utils import decode_bytes, PyV3
    import internals.output_collector as OutputCollector
    import symbols
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.logging as Logging
    import SublimeHaskell.internals.settings as Settings
    from SublimeHaskell.internals.utils import decode_bytes, PyV3
    import SublimeHaskell.internals.output_collector as OutputCollector
    import SublimeHaskell.symbols as symbols

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# It also eats whitespace before the first line.
# The first line is divided into a filename, a line number, and a column.
output_regex = re.compile(
    r'\s*^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)',
    re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
result_file_regex = r'^\s{2}(\S*?): line (\d+), column (\d+):$'

OUTPUT_PANEL_NAME = 'sublime_haskell_output_panel'

BUILD_LOG_PANEL_NAME = 'sublime_haskell_build_log_panel'

# Global list of errors.
ERRORS = []

# Global ref to view with errors
error_view = None


def filename_of_path(p):
    """Returns everything after the last slash or backslash."""
    # Not using os.path here because we don't know/care here if
    # we have forward or backslashes on Windows.
    return re.match(r'(.*[/\\])?(.*)', p).groups()[1]


class OutputMessage(object):
    "Describe an error or warning message produced by GHC."
    def __init__(self, filename, region, message, level, correction = None):
        self.filename = filename
        self.region = region
        self.message = message.replace(os.linesep, "\n")
        self.level = level
        self.correction = correction

    def __unicode__(self):
        # must match result_file_regex
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
    for e in ERRORS:
        e.erase_from_view()
    ERRORS = []


def set_global_error_messages(messages):
    global ERRORS
    clear_error_marks()
    ERRORS.extend(messages)


def run_build_thread(view, cabal_project_dir, msg, cmd, on_done):
    run_chain_build_thread(view, cabal_project_dir, msg, [cmd], on_done)


def run_chain_build_thread(view, cabal_project_dir, msg, cmds, on_done):
    Common.show_status_message_process(msg, priority = 3)
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
    collected_out = []
    output_log = Common.output_panel(view.window(), '', panel_name=BUILD_LOG_PANEL_NAME, show_panel=Settings.get_setting_async('show_output_window'))
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
    for m in messages:
        summary[m.level] = summary[m.level] + 1
    summary_line = 'Errors: {0}, Warnings: {1}, Hints: {2}'.format(
        summary['error'],
        summary['warning'],
        summary['hint'])

    def messages_level(name, level):
        if PyV3:
            if not summary[level]:
                return ''
            count = '{0}: {1}'.format(name, summary[level])
            msgs = '\n'.join(str(m) for m in messages if m.level == level)
            return '{0}\n\n{1}'.format(count, msgs)
        else:
            if not summary[level]:
                return u''
            count = u'{0}: {1}'.format(name, summary[level])
            msgs = u'\n'.join(unicode(m) for m in messages if m.level == level)
            return u'{0}\n\n{1}'.format(count, msgs)

    errors = messages_level('Errors', 'error')
    warnings = messages_level('Warnings', 'warning')
    hints = messages_level('Hints', 'hint')

    parts = filter(lambda s: s, [summary_line, errors, warnings, hints])
    if PyV3:
        return '\n\n'.join(parts)
    else:
        return u'\n\n'.join(parts)


def show_output_result_text(view, msg, text, exit_code, base_dir):
    """Shows text (formatted messages) in output with build result"""

    success = exit_code == 0

    success_message = 'SUCCEEDED' if success else 'FAILED'
    output = u'Build {0}\n\n{1}'.format(success_message, text.strip())

    Common.show_status_message_process(msg, success)
    # Show panel if there is any text to show (without the part that we add)
    if text:
        if Settings.get_setting_async('show_error_window'):
            sublime.set_timeout(lambda: write_output(view, output, base_dir), 0)


def parse_output_messages_and_show(view, msg, base_dir, exit_code, stderr):
    """Parse errors and display resulting errors"""

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
            outputs += ['', '']
    if unparsable:
        outputs += ["Collected output:\n", unparsable]

    show_output_result_text(view, msg, '\n'.join(outputs), exit_code, base_dir)
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
    Logging.log('total time to mark {0} diagnostics: {1} seconds'.format(
        len(errors), end_time - begin_time), Logging.LOG_DEBUG)


message_levels = {
    'hint': {
        'style': 'sublimehaskell.mark.hint',
        'icon': {
            'normal': 'haskell-hint.png',
            'fix': 'haskell-hint-fix.png' }
    },
    'warning': {
        'style': 'sublimehaskell.mark.warning',
        'icon': {
            'normal': 'haskell-warning.png',
            'fix': 'haskell-warning-fix.png' }
    },
    'error': {
        'style': 'sublimehaskell.mark.error',
        'icon': {
            'normal': 'haskell-error.png',
            'fix': 'haskell-error-fix.png' }
    }
}


def errors_for_view(view):
    errs = []
    for e in ERRORS:
        if os.path.samefile(e.filename, view.file_name()):
            e.update_region()
            errs.append(e)
    return sorted(errs, key = lambda e: e.region)


def update_messages_in_view(view, errors):
    mark_messages_in_view(errors, view)


# These next and previous commands were shamelessly copied
# from the great SublimeClang plugin.


def get_error_at(filename, line, column):
    global ERRORS
    for err in ERRORS:
        if err.filename == filename and err.region.start.line == line and err.region.start.column :
            return err
    return None


def goto_error(view, error):
    line = error.region.start.line + 1
    column = error.region.start.column + 1
    filename = error.filename
    global error_view
    if error_view:
        show_output(view)
        # error_region = error_view.find('{0}: line {1}, column \\d+:(\\n\\s+.*)*'.format(re.escape(filename), line), 0)
        error_region = error_view.find(re.escape(str(error)), 0)
        # error_region = error_view.find('\\s{{2}}{0}: line {1}, column {2}:(\\n\\s+.*)*'.format(re.escape(filename), line, column), 0)
        error_view.add_regions("current_error", [error_region], 'string', 'dot', sublime.HIDDEN)
        error_view.show(error_region.a)
    view.window().open_file("{0}:{1}:{2}".format(filename, line, column), sublime.ENCODED_POSITION)


def get_next_value(lst, p, cycle = True):
    # Get first value satisfying `p`, if no such values and `cycle`, return first element
    for x in filter(p, lst):
        return x
    if cycle and len(lst):
        return lst[0]
    return None


def get_prev_value(lst, p, cycle = True):
    # Inverse of `get_next_value`, goes back and finds value satisfying `p`
    return get_next_value(reversed(lst), p, cycle)


class SublimeHaskellNextError(Common.SublimeHaskellTextCommand):
    def run(self, edit):
        errs = errors_for_view(self.view)
        if not errs:
            Common.show_status_message('No errors or warnings!', priority = 5)
        next_err = get_next_value(errs, lambda e: e.region > v.sel()[0])
        v.sel().clear()
        v.sel().add(next_err.region.to_region(self.view))
        goto_error(self.view, next_err)


class SublimeHaskellPreviousError(Common.SublimeHaskellTextCommand):
    def run(self, edit):
        errs = errors_for_view(self.view)
        if not errs:
            Common.show_status_message("No errors or warnings!", priority = 5)
        prev_err = get_prev_value(errs, lambda e: e.region < v.sel()[0])
        v.sel().clear()
        v.sel().add(prev_err.region.to_region(self.view))
        goto_error(self.view, prev_err)


def region_key(name, is_fix = False):
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
    for m in messages:
        m.erase_from_view()

    for i, m in enumerate(messages):
        m.region.save(view, '{0}-{1}'.format(region_key(m.level, m.correction is not None), str(i)))
        view.add_regions(
            m.region.region_key,
            [m.to_region(view)],
            message_levels[m.level]['style'],
            get_icon(message_levels[m.level]['icon']['fix' if m.correction is not None else 'normal']),
            sublime.DRAW_OUTLINED)
        if m.correction and m.correction.corrector:
            m.correction.corrector.region.save(view, 'autofix-{0}'.format(str(i)))
            view.add_regions(
                m.correction.corrector.region.region_key,
                [m.correction.corrector.region.to_region(view)],
                'autofix.region',
                '',
                sublime.HIDDEN)


def write_output(view, text, cabal_project_dir, show_panel = True):
    "Write text to Sublime's output panel."
    global error_view
    error_view = Common.output_panel(view.window(), text, panel_name = OUTPUT_PANEL_NAME, syntax = 'HaskellOutputPanel', show_panel = show_panel)
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
            symbols.Region(
                symbols.Position(line, column),
                symbols.Position(line, column)),
            messy_details.strip(),
            'warning' if 'warning' in messy_details.lower() else 'error')

    return list(map(to_error, matches))


def trim_region(view, region):
    "Return the specified Region, but without leading or trailing whitespace."
    text = view.substr(region)
    # Regions may be selected backwards, so b could be less than a.
    a = region.begin()
    b = region.end()
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
