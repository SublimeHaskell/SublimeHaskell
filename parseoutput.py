# -*- coding: UTF-8 -*-

import os
import os.path
import re
import time

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.hsdev.result_parse as HsResultParse
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.regexes as Regexes
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.symbols as Symbols

OUTPUT_PANEL_NAME = 'sublime_haskell_output_panel'


def filename_of_path(path):
    """Returns everything after the last slash or backslash."""
    # Not using os.path here because we don't know/care here if
    # we have forward or backslashes on Windows.
    return re.match(r'(.*[/\\])?(.*)', path).groups()[1]


class OutputMessage(object):
    '''Describe an error or warning message produced by GHC.
    '''
    def __init__(self, view, filename, region, message, level, correction=None):
        super().__init__()
        self.view = view
        self._filename = filename
        self.region = region
        self.message = message
        self.level = level
        self.correction = correction

    def __unicode__(self):
        # must match RESULT_FILE_REGEX
        # TODO: Columns must be recalculated, such that one tab is of tab_size length
        # We can do this for opened views, but how to do this for files that are not open?
        if self.region is not None:
            retval = u'  {0}: line {1}, column {2}:\n    {3}'.format(self.filename, self.region.start.line + 1,
                                                                     self.region.start.column + 1, self.message)
        else:
            retval = u'  {0}:\n    {1}'.format(self.filename, self.message)

        return retval

    @property
    def filename(self):
        return self._filename

    @filename.setter
    def filename(self, _value):
        # Cannot assign the file name. :-)
        pass


    def __str__(self):
        return self.__unicode__()

    def __repr__(self):
        return '<OutputMessage {0}:{1}-{2}: {3}>'.format(filename_of_path(self.view.file_name()),
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

    def updated(self):
        self.update_region()
        return self

class MarkerManager(object):
    '''Convert and display errors, warnings and autofix/hint markers produced by the SublimeHaskell backend and build
    outputs.
    '''

    MESSAGE_LEVELS = {
        'hint': {
            'style': 'sublimehaskell.mark.hint',
            'icon': {'normal': 'haskell-hint.png',
                     'fix': 'haskell-hint-fix.png'}
        },
        'warning': {'style': 'sublimehaskell.mark.warning',
                    'icon': {'normal': 'haskell-warning.png',
                             'fix': 'haskell-warning-fix.png'}
                   },
        'error': {'style': 'sublimehaskell.mark.error',
                  'icon': {'normal': 'haskell-error.png',
                           'fix': 'haskell-error-fix.png'}
                 },
        'uncategorized': {'style': 'sublimehaskell.mark.warning',
                          'icon': {'normal': 'haskell-warning.png',
                                   'fix': 'haskell-warning-fix.png'}
                         }
    }

    def __init__(self):
        super().__init__()
        self.error_marks = []
        self.messages = []
        self.message_panel = None

    def mark_response(self, view, msgs, corrections, fly_mode):
        '''Generate a list of :py:class:`OutputMessage` objects from a backend response.
        '''
        self.clear_error_marks()

        def to_output_message(msg):
            filename = msg.get('source', {}).get('file', '<no file/command line/OPTIONS_GHC>')
            file_view = view.window().find_open_file(filename)
            if msg.get('region'):
                region = HsResultParse.parse_region(msg.get('region')).to_zero_based()
            else:
                region = Symbols.Region(0)
            level = msg.get('level', 'uncategorized')
            caption = level.capitalize() + ': ' + msg.get('note', {}).get('message', '')
            caption.replace('\n', '\n  ')

            return OutputMessage(file_view, filename, region, caption, level)

        self.messages = [to_output_message(msg) for msg in msgs]
        self.limit_messages(view)

        # Hack alert: Region and Position.to_zero_based() return the original object (self) after updating it.
        # 'and' returns the right hand side, which is never None or a false value.
        #
        # Bascially, a Pythonic way to make side effects happen and still get a list of things we want.
        corrections_dict = dict(((os.path.normpath(c.file), c.message_region.start.line, c.message_region.start.column), c)
                                for c in [corr.message_region.to_zero_based() and corr for corr in corrections or []])

        for omsg in self.messages:
            okey = (os.path.normpath(omsg.filename), omsg.region.start.line, omsg.region.start.column)
            if okey in corrections_dict:
                omsg.correction = corrections_dict[okey]

        self.error_marks.extend(self.messages)

        if Settings.PLUGIN.show_error_window:
            filename = view.file_name()
            cabal_proj_dir = Common.get_cabal_project_dir_of_file(filename) or os.path.dirname(filename)
            show_panel = not fly_mode and self.messages
            output_text = self.format_output_messages()
            sublime.set_timeout(lambda: self.make_message_panel(view, output_text, cabal_proj_dir, show_panel), 0)

        sublime.set_timeout(self.update_markers_across_views, 0)


    def mark_compiler_output(self, view, banner, base_dir, text, exit_code):
        '''Parse text into a list of OutputMessage objects.
        '''

        def to_error(errmsg):
            filename, line, column, messy_details = errmsg.groups()
            line, column = int(line), int(column)

            column = ghc_column_to_sublime_column(view, line, column)
            line = line - 1
            # Record the absolute, normalized path.
            return OutputMessage(view.window().find_open_file(filename),
                                 os.path.normpath(os.path.join(base_dir, filename)),
                                 Symbols.Region(Symbols.Position(line, column)),
                                 messy_details.strip(),
                                 'warning' if 'warning' in messy_details.lower() else 'error')

        self.messages = [to_error(err) for err in Regexes.OUTPUT_REGEX.finditer(text)]
        self.limit_messages(view)

        output_text = self.format_output_messages()
        unparsed = Regexes.OUTPUT_REGEX.sub('', text).strip()
        if unparsed:
            output_text += '\n\nAdditional output:\n------------------\n' + unparsed

        self.clear_error_marks()
        self.error_marks.extend(self.messages)

        if Settings.PLUGIN.show_error_window and self.messages:
            output_text = u'{0} {1}\n\n{2}'.format(banner, u'SUCCEEDED' if exit_code == 0 else u'FAILED', output_text)
            sublime.set_timeout(lambda: self.make_message_panel(view, output_text, base_dir, True), 0)

        sublime.set_timeout(self.update_markers_across_views, 0)


    def format_output_messages(self):
        """Formats list of messages"""
        summary = {'error': 0, 'warning': 0, 'hint': 0, 'uncategorized': 0}
        for msg in self.messages:
            summary[msg.level] = summary[msg.level] + 1
        summary_line = 'Errors: {0}, Warnings: {1}, Hints: {2}, Uncategorized {3}'.format(summary['error'],
                                                                                          summary['warning'],
                                                                                          summary['hint'],
                                                                                          summary['uncategorized'])

        def messages_level(name, level):
            if summary[level]:
                count = '{0}: {1}'.format(name, summary[level])
                msgs = '\n'.join(str(m) for m in self.messages if m.level == level)
                return '{0}\n\n{1}'.format(count, msgs)

            return ''

        errors = messages_level('Errors', 'error')
        warnings = messages_level('Warnings', 'warning')
        hints = messages_level('Hints', 'hint')
        uncategorized = messages_level('Uncategorized', 'uncategorized')

        return '\n\n'.join(filter(lambda s: s, [summary_line, errors, warnings, hints, uncategorized]))


    def limit_messages(self, view):
        show_only = Settings.get_project_setting(view, 'show_only', Settings.PLUGIN.show_only)
        show_errors = show_only.get('errors', True)
        show_warnings = show_only.get('warnings', True)
        show_hints = show_only.get('hints', True)

        if not show_errors or not show_warnings or not show_hints:
            self.messages = [msg for msg in self.messages if (msg.level == 'error' and show_errors) or \
                                                             (msg.level == 'warning' and show_warnings) or \
                                                             (msg.level == 'hint' and show_hints)]

    def clear_error_marks(self):
        for err in self.error_marks:
            err.erase_from_view()
        self.error_marks = []


    def update_markers_across_views(self):
        '''Mark the regions in open views where errors were found.
        '''
        begin_time = time.clock()
        for win in sublime.windows():
            for view in win.views():
                self.update_markers_in_view(view)
        end_time = time.clock()
        Logging.log('total time to mark {0} diagnostics: {1} seconds'.format(len(self.messages), end_time - begin_time),
                    Logging.LOG_DEBUG)


    def update_markers_in_view(self, view):
        def region_key(name, is_fix):
            return 'output-{0}s{1}'.format(name, '' if not is_fix else '-fix')


        def get_icon(png):
            return '/'.join(["Packages", os.path.basename(os.path.dirname(__file__)), "Icons", png])

        view_specific = self.marks_for_view(view)
        if not view_specific:
            return

        for msg in view_specific:
            msg.erase_from_view()

        for i, msg in enumerate(view_specific):
            msg.region.save(view, '{0}-{1}'.format(region_key(msg.level, msg.correction is not None), str(i)))
            view.add_regions(msg.region.region_key,
                             [msg.to_region(view)],
                             self.MESSAGE_LEVELS[msg.level]['style'],
                             get_icon(self.MESSAGE_LEVELS[msg.level]['icon']['fix' if msg.correction else 'normal']),
                             sublime.DRAW_OUTLINED)

            if msg.correction and msg.correction.corrector:
                msg.correction.corrector.region.save(view, 'autofix-{0}'.format(str(i)))
                view.add_regions(msg.correction.corrector.region.region_key,
                                 [msg.correction.corrector.region.to_region(view)],
                                 'autofix.region',
                                 '',
                                 sublime.HIDDEN)


    def marks_for_view(self, view):
        return sorted([mark.updated() for mark in self.error_marks if mark.view == view], key=lambda e: e.region)


    def apply_autocorrect(self, view, rgn):
        # repl_text needs visibility scope outside of the loop.
        repl_text = None

        for err in [mark for mark in self.marks_for_view(view) if mark.correction and mark.correction.corrector.region == rgn]:
            err.erase_from_view()
            self.error_marks.remove(err)

            corrector = err.correction.corrector
            err_rgn = corrector.to_region(view)
            err_text = corrector.contents

            repl_text = {'text': err_text,
                         'begin': err_rgn.begin(),
                         'end': err_rgn.end()}

            sublime.set_timeout(lambda: view.run_command('sublime_haskell_replace_text', repl_text), 0)


    def make_message_panel(self, view, text, cabal_project_dir, panel_out):
        '''Create the message panel for error/warnings/hints/uncategorized errors'''
        self.message_panel = Common.output_panel(view.window(), text, panel_name=OUTPUT_PANEL_NAME, syntax='HaskellOutputPanel',
                                                 panel_display=panel_out)
        self.message_panel.settings().set("result_file_regex", Regexes.RESULT_FILE_REGEX)
        self.message_panel.settings().set("result_base_dir", cabal_project_dir)


MARKER_MANAGER = MarkerManager()
'''The global marker manager object.
'''


# These next and previous commands were shamelessly copied
# from the great SublimeClang plugin.


def goto_error(view, mark):
    line = mark.region.start.line + 1
    column = mark.region.start.column + 1

    show_output(view)
    msg_panel = MARKER_MANAGER.message_panel
    # error_region = msg_panel.find('{0}: line {1}, column \\d+:(\\n\\s+.*)*'.format(re.escape(mark.filename), line), 0)
    error_region = msg_panel.find(re.escape(str(mark)), 0)
    # error_region = msg_panel.find('\\s{{2}}{0}: line {1}, column {2}:(\\n\\s+.*)*'.format(re.escape(mark.filename),
    #                                                                                        line, column), 0)
    msg_panel.add_regions("current_error", [error_region], 'string', 'dot', sublime.HIDDEN)
    msg_panel.show(error_region.a)

    view.window().open_file("{0}:{1}:{2}".format(mark.filename, line, column), sublime.ENCODED_POSITION)


class SublimeHaskellNextError(CommandWin.SublimeHaskellTextCommand):
    def run(self, _edit, **_kwargs):
        errs = MARKER_MANAGER.marks_for_view(self.view)
        if not errs:
            Common.sublime_status_message('No errors or warnings!')
        else:
            view_pt = self.view.sel()[0]
            # Bump just past the view's point, just in case we're sitting on top of the current
            cur_point = Symbols.Region.from_region(self.view, view_pt)
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


class SublimeHaskellPreviousError(CommandWin.SublimeHaskellTextCommand):
    def run(self, _edit, **_kwargs):
        errs = MARKER_MANAGER.marks_for_view(self.view)
        if not errs:
            Common.sublime_status_message("No errors or warnings!")
        else:
            cur_point = Symbols.Region.from_region(self.view, self.view.sel()[0])
            prev_err = next(filter(lambda e: e.region < cur_point, reversed(errs)), None)
            # Cycle around to the last error if we run off the first
            if prev_err is None:
                prev_err = errs[-1]
            self.view.sel().clear()
            self.view.sel().add(prev_err.region.to_region(self.view))
            goto_error(self.view, prev_err)


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
    Parses result of :i <name> command of ghci and returns derived Symbols.Declaration
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
                return Symbols.Class(name, ctx, args)
            elif what == 'data':
                return Symbols.Data(name, ctx, args, definition)
            elif what == 'type':
                return Symbols.Type(name, ctx, args, definition)
            elif what == 'newtype':
                return Symbols.Newtype(name, ctx, args, definition)
            else:
                raise RuntimeError('Unknown type of symbol: {0}'.format(what))

    else:
        # function
        function_regex = r'{0}\s+::\s+(?P<type>.*?)(\s+--(.*))?$'.format(name)
        matched = re.search(function_regex, contents, re.MULTILINE)
        if matched:
            return Symbols.Function(name, matched.group('type'))

    return None
