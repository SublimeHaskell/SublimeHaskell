# -*- coding: UTF-8 -*-

"""Haskell type display and query support"""

import re
from functools import total_ordering

import sublime
import sublime_plugin

import SublimeHaskell.ghci_backend as GHCIMod
import SublimeHaskell.hdevtools as HDevTools
import SublimeHaskell.hsdev.agent as hsdev
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.unicode_opers as UnicodeOpers
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.symbols as Symbols

# Used to find out the module name.
MODULE_RE_STR = r'module\s+([^\s\(]*)'  # "module" followed by everything that is neither " " nor "("
MODULE_RE = re.compile(MODULE_RE_STR)

# Parses the output of `ghc-mod type`.
# Example: 39 1 40 17 "[Char]"
GHCMOD_TYPE_LINE_RE = re.compile(r'(?P<startrow>\d+) (?P<startcol>\d+) (?P<endrow>\d+) (?P<endcol>\d+) "(?P<type>.*)"')

# Name of the sublime panel in which type information is shown.
TYPES_PANEL_NAME = 'sublime_haskell_show_types_panel'


def parse_ghc_mod_type_line(line):
    """
    Returns the `groupdict()` of GHCMOD_TYPE_LINE_RE matching the given line,
    of `None` if it doesn't match.
    """
    match = GHCMOD_TYPE_LINE_RE.match(line)
    return match and match.groupdict()


@total_ordering
class FilePosition(object):
    """
    Zero-based sublime file position
    """

    def __init__(self, line, column):
        self.line = line
        self.column = column

    def __eq__(self, other):
        return self.line == other.line and self.column == other.column

    def __lt__(self, other):
        if self.line == other.line:
            return self.column < other.column
        else:
            return self.line < other.line

    def point(self, view):
        return view.text_point(self.line, self.column)

    def to_str(self):
        return '{0}:{1}'.format(self.line, self.column)

    @staticmethod
    def from_point(view, point):
        (row, col) = view.rowcol(point)
        return FilePosition(int(row), int(col))

    @staticmethod
    def from_type_pos(view, row, col):
        return FilePosition(int(row) - 1, ParseOutput.ghc_column_to_sublime_column(view, int(row), int(col)))

    # From one-based line-column
    @staticmethod
    def from_str(posn):
        if not posn:
            return None
        [row, col] = posn.split(':')
        return FilePosition(int(row - 1), int(col - 1))


def position_by_point(view, point):
    return FilePosition.from_point(view, point)


class RegionType(object):
    def __init__(self, typename, start, end=None):
        self.typename = typename
        self.start = start
        self.end = end if end else start

    def region(self, view):
        return sublime.Region(self.start.point(view), self.end.point(view))

    def substr(self, view):
        return view.substr(self.region(view))

    @Symbols.unicode_operators
    def show(self, view):
        expr = self.substr(view)
        fmt = '{0} :: {1}' if len(expr.splitlines()) == 1 else '{0}\n\t:: {1}'
        return fmt.format(self.substr(view), self.typename)

    def precise_in_region(self, view, other):
        this_region = self.region(view)
        other_region = other.region(view)
        if other_region.contains(this_region):
            return (0, other_region.size() - this_region.size())
        elif other_region.intersects(this_region):
            return (1, -other_region.intersection(this_region).size())
        return (2, 0)


class TypedRegion(object):
    def __init__(self, view, region, typename):
        self.view = view
        self.expr = view.substr(region)
        self.region = region
        self.typename = typename

    @Symbols.unicode_operators
    def show(self, _view):
        fmt = '{0} :: {1}' if len(self.expr.splitlines()) == 1 else '{0}\n\t:: {1}'
        return fmt.format(self.expr, self.typename)

    def __eq__(self, r):
        return self.region == r.region

    def contains(self, rgn):
        return self.contains_region(rgn.region)

    def contains_region(self, rgn):
        return self.region.contains(rgn)

    @staticmethod
    def from_region_type(rgn, view):
        return TypedRegion(view, rgn.region(view), rgn.typename)


def region_by_region(view, region, typename):
    return RegionType(typename, position_by_point(view, region.a), position_by_point(view, region.b))

TYPE_RE = re.compile(r'(?P<line1>\d+)\s+(?P<col1>\d+)\s+(?P<line2>\d+)\s+(?P<col2>\d+)\s+"(?P<type>.*)"$')


def parse_type_output(view, msg):
    result = []
    for line in msg.splitlines():
        matched = TYPE_RE.match(line)
        if matched:
            result.append(RegionType(matched.group('type'),
                                     FilePosition.from_type_pos(view, int(matched.group('line1')), int(matched.group('col1'))),
                                     FilePosition.from_type_pos(view, int(matched.group('line2')), int(matched.group('col2')))))

    return result


def sorted_types(view, types, point):
    return sorted(list(filter(lambda t: t.region(view).contains(point), types)),
                  key=lambda t: t.region(view).size())


def get_type(view, filename, module_name, line, column, cabal=None):
    result = None

    if Settings.PLUGIN.enable_hsdev:
        # Convert from hsdev one-based locations to sublime zero-based positions
        return sorted_types(view, get_types(filename) or [], FilePosition(line, column).point(view))

    column = ParseOutput.sublime_column_to_ghc_column(view, line, column)
    line = line + 1
    if Settings.PLUGIN.enable_hdevtools:
        result = HDevTools.hdevtools_type(filename, line, column, cabal=cabal)
    if not result and module_name and Settings.PLUGIN.enable_ghc_mod:
        result = GHCIMod.ghcmod_type(filename, module_name, line, column)

    return parse_type_output(view, result) if result else None


def get_type_view(view, selection=None):
    filename = view.file_name()

    if selection is None:
        selection = view.sel()[0]

    line, column = view.rowcol(selection.b)
    module_name = Utils.head_of(hsdev.client.module(file=filename))

    return get_type(view, filename, module_name, line, column)


def get_types(filename, on_result=None):
    if Settings.PLUGIN.enable_hsdev:
        def to_file_pos(rgn):
            return FilePosition(int(rgn['line']) - 1, int(rgn['column']) - 1)

        def to_region_type(rgn):
            return RegionType(rgn['note']['type'],
                              to_file_pos(rgn['region']['from']),
                              to_file_pos(rgn['region']['to']))

        def on_resp(resps):
            types = [to_region_type(r) for r in resps]
            FILE_TYPES.set(filename, types, False)
            on_result(types)

        if FILE_TYPES.has(filename):
            return FILE_TYPES.get(filename)

        wait = on_result is None
        res = hsdev.client.types(files=[filename],
                                 ghc=GHCIMod.get_ghc_opts(filename),
                                 wait=wait, on_response=on_resp if on_result is not None else None)
        if res is not None and wait:
            types = [to_region_type(r) for r in res]
            FILE_TYPES.set(filename, types, False)
            return types


class SublimeHaskellShowType(Common.SublimeHaskellTextCommand):
    def __init__(self, view):
        super().__init__(view)

        self.types = None
        self.output_view = None

    def run(self, edit, **kwargs):
        filename = kwargs.get('filename')
        line = kwargs.get('line')
        column = kwargs.get('column')

        result = self.get_types(filename, int(line) if line else None, int(column) if column else None)
        self.show_types(result)

    def get_types(self, filename=None, line=None, column=None):
        if not filename:
            filename = self.view.file_name()

        if (not line) or (not column):
            line, column = self.view.rowcol(self.view.sel()[0].b)

        module_name = Utils.head_of(hsdev.client.module(file=filename))

        return get_type(self.view, filename, module_name, line, column)

    def get_best_type(self, types):
        if not types:
            return None

        region = self.view.sel()[0]
        file_region = region_by_region(self.view, region, '')
        if region.a != region.b:
            return sorted(types, key=lambda r: file_region.precise_in_region(self.view, r))[0]
        else:
            return types[0]

    def show_types(self, types):
        if not types:
            Common.show_status_message("Can't infer type", False)
            return

        self.types = types
        self.output_view = Common.output_panel(self.view.window(), '',
                                               panel_name=TYPES_PANEL_NAME,
                                               syntax='Haskell-SublimeHaskell')
        self.view.window().show_quick_panel([t.typename for t in self.types], self.on_done, 0, -1, self.on_changed)

    def on_done(self, idx):
        self.view.erase_regions('typed')

        if idx == -1:
            return

        typ = self.types[idx]
        Common.output_text(self.output_view, typ.show(self.view), clear=True)

    def on_changed(self, idx):
        if idx == -1:
            return

        typ = self.types[idx]
        Common.output_text(self.output_view, typ.show(self.view), clear=True)
        self.view.add_regions('typed', [typ.region(self.view)], 'string', 'dot', sublime.DRAW_OUTLINED)

    def is_enabled(self):
        return Common.is_enabled_haskell_command(self.view, False)


class FileTypes(object):
    def __init__(self):
        self.types = {}
        self.status = {}

    def set(self, filename, types, show=True):
        self.types[filename] = types
        self.status[filename] = show

    def remove(self, filename):
        if self.has(filename):
            del self.types[filename]
            del self.status[filename]

    def get(self, filename):
        return self.types.get(filename)

    def has(self, filename):
        return filename in self.types

    def shown(self, filename):
        return self.status.get(filename, False)

    def show(self, filename):
        self.status[filename] = True

    def hide(self, filename):
        self.status[filename] = False

FILE_TYPES = FileTypes()


class SublimeHaskellShowTypes(SublimeHaskellShowType):
    def run(self, edit, **kwargs):
        filename = kwargs.get('filename')
        line = kwargs.get('line')
        column = kwargs.get('column')

        result = self.get_types(filename, int(line) if line else None, int(column) if column else None)
        self.show_types(result)

    def show_types(self, types):
        if not types:
            Common.show_status_message("Can't infer type", False)
            return

        self.types = types
        self.output_view = Common.output_panel(self.view.window(), '',
                                               panel_name=TYPES_PANEL_NAME,
                                               syntax='Haskell-SublimeHaskell',
                                               panel_display=False)
        regions = []
        for typ in self.types:
            Common.output_text(self.output_view, '{0}\n'.format(typ.show(self.view)), clear=False)
            regions.append(sublime.Region(self.output_view.size() - 1 - len(UnicodeOpers.use_unicode_operators(typ.typename)),
                                          self.output_view.size() - 1))
        self.output_view.add_regions('types', regions, 'comment', '', sublime.DRAW_OUTLINED)
        Common.show_panel(self.view.window(), panel_name=TYPES_PANEL_NAME)


class SublimeHaskellGetTypes(Common.SublimeHaskellTextCommand):
    def __init__(self, view):
        super().__init__(view)

        self.filename = None

    def run(self, edit, **kwargs):
        self.filename = kwargs.get('filename')
        if not self.filename:
            self.filename = self.view.file_name()
        if not FILE_TYPES.has(self.filename):
            get_types(self.filename, self.on_types)

    def on_types(self, types):
        pass


class SublimeHaskellShowAllTypes(Common.SublimeHaskellTextCommand):
    def __init__(self, view):
        super().__init__(view)

        self.filename = None
        self.output_view = None

    def run(self, edit, **kwargs):
        self.filename = kwargs.get('filename')
        if not self.filename:
            self.filename = self.view.file_name()
        if not FILE_TYPES.has(self.filename):
            get_types(self.filename, self.on_types)
        else:
            self.on_types(FILE_TYPES.get(self.filename))

    def on_types(self, types):
        FILE_TYPES.show(self.filename)
        self.show_types(types)

    def show_types(self, types):
        if not types:
            Common.show_status_message("Can't infer type", False)
            return

        types = sorted(
            list(filter(lambda t: t.region(self.view).contains(self.view.sel()[0]), types)),
            key=lambda t: t.region(self.view).size())
        self.output_view = Common.output_panel(self.view.window(), '',
                                               panel_name=TYPES_PANEL_NAME,
                                               syntax='Haskell-SublimeHaskell',
                                               panel_display=False)

        regions = []
        for typ in types:
            Common.output_text(self.output_view, '{0}\n'.format(typ.show(self.view)), clear=False)
            regions.append(sublime.Region(self.output_view.size() - 1 - len(UnicodeOpers.use_unicode_operators(typ.typename)),
                                          self.output_view.size() - 1))
        self.output_view.add_regions('types', regions, 'comment', '', sublime.DRAW_OUTLINED)
        Common.show_panel(self.view.window(), panel_name=TYPES_PANEL_NAME)

    def is_enabled(self):
        return Common.is_haskell_source(self.view) and self.view.file_name() is not None


class SublimeHaskellHideAllTypes(Common.SublimeHaskellTextCommand):
    def run(self, edit):
        FILE_TYPES.hide(self.view.file_name())
        Common.hide_panel(self.view.window(), panel_name=TYPES_PANEL_NAME)

    def is_enabled(self):
        return Common.is_haskell_source(self.view) and \
               self.view.file_name() is not None and \
               FILE_TYPES.has(self.view.file_name()) and \
               FILE_TYPES.shown(self.view.file_name())


class SublimeHaskellToggleAllTypes(Common.SublimeHaskellTextCommand):
    def run(self, edit):
        if FILE_TYPES.shown(self.view.file_name()):
            self.view.run_command('sublime_haskell_hide_all_types')
        else:
            self.view.run_command('sublime_haskell_show_all_types')

    def is_enabled(self):
        return Common.is_haskell_source(self.view) and self.view.file_name() is not None


# Works only with the cursor being in the name of a toplevel function so far.
class SublimeHaskellInsertType(SublimeHaskellShowType):
    def run(self, edit):
        result = self.get_best_type(self.get_types())
        if result:
            res = result.region(self.view)
            qsymbol = Common.get_qualified_symbol_at_region(self.view, self.view.word(res.begin()))
            line_begin = self.view.line(res).begin()
            prefix = self.view.substr(sublime.Region(line_begin, res.begin()))
            indent = re.search(r'(?P<indent>\s*)', prefix).group('indent')
            signature = '{0}{1} :: {2}\n'.format(indent, qsymbol.name, result.typename)
            self.view.insert(edit, line_begin, signature)


class ExpandSelectionInfo(object):
    def __init__(self, view, selection=None):
        self.view = view
        self.selection = selection if selection is not None else view.sel()[0]
        if FILE_TYPES.has(self.view.file_name()):
            types = sorted_types(self.view, FILE_TYPES.get(self.view.file_name()), self.selection.b)
        else:
            types = get_type_view(self.view, self.selection)
        self.regions = [TypedRegion.from_region_type(t, view) for t in types] if types else None
        self.expanded_index = None

    def is_valid(self):
        return self.regions is not None

    def is_actual(self, view=None, selection=None):
        if not view:
            view = self.view
        if selection is None:
            selection = self.selection
        return self.view == view and self.selection == selection

    def is_top(self):
        if self.expanded_index is None:
            return False
        return self.expanded_index + 1 == len(self.regions)

    def typed_region(self):
        return self.regions[self.expanded_index] if self.expanded_index is not None else None

    def expand(self):
        if not self.is_valid():
            return None
        if self.is_top():
            return self.typed_region()

        if self.expanded_index is None:
            for i, rgn in enumerate(self.regions):
                if rgn.contains_region(self.selection) and rgn.region != self.selection:
                    self.expanded_index = i
                    break
        else:
            self.expanded_index = self.expanded_index + 1

        self.selection = self.typed_region().region
        return self.typed_region()


# Expand selection to expression
class SublimeHaskellExpandSelectionExpression(SublimeHaskellShowType):
    # last expand regions with type
    Infos = None

    def __init__(self, view):
        super().__init__(view)

    def run(self, edit):
        selections = list(self.view.sel())

        if not self.is_infos_valid(selections):
            SublimeHaskellExpandSelectionExpression.Infos = [ExpandSelectionInfo(self.view, s) for s in selections]

        if not self.is_infos_valid(selections):
            Common.show_status_message('Unable to retrieve expand selection info', False)
            return

        selinfo = [i.expand() for i in self.Infos]
        self.view.sel().clear()
        self.view.sel().add_all([sel.region for sel in selinfo])

        Common.output_panel(self.view.window(),
                            '\n'.join([UnicodeOpers.use_unicode_operators(sel.typename) for sel in selinfo]),
                            panel_name='sublime_haskell_expand_selection_expression',
                            syntax='Haskell-SublimeHaskell')

    def is_infos_valid(self, selections):
        return self.Infos and \
               all([i.is_valid() for i in self.Infos]) and \
               len(selections) == len(self.Infos) and \
               all([i.is_actual(self.view, s) for i, s in zip(self.Infos, selections)])


class SublimeHaskellTypes(sublime_plugin.EventListener):
    def on_selection_modified(self, view):
        if Common.is_haskell_source(view) and \
           view.file_name() and FILE_TYPES.has(view.file_name()) and \
           FILE_TYPES.shown(view.file_name()):
            view.run_command('sublime_haskell_show_all_types', {'filename': view.file_name()})

    def on_modified(self, view):
        FILE_TYPES.remove(view.file_name())
