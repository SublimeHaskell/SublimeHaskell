# -*- coding: UTF-8 -*-

"""Haskell type display and query support"""

import sublime
import sublime_plugin
import re
from functools import total_ordering

if int(sublime.version()) < 3000:
    from sublime_haskell_common import is_enabled_haskell_command, show_status_message, SublimeHaskellTextCommand, output_panel, output_text, get_ghc_opts, is_haskell_source, show_panel, hide_panel, head_of
    from internals.settings import get_setting_async
    from autocomplete import get_qualified_symbol_at_region
    import hsdev
    from hdevtools import hdevtools_type
    from check_lint import ghcmod_type
    from parseoutput import sublime_column_to_ghc_column, ghc_column_to_sublime_column
    from symbols import unicode_operators, use_unicode_operators
else:
    from SublimeHaskell.sublime_haskell_common import is_enabled_haskell_command, show_status_message, SublimeHaskellTextCommand, output_panel, output_text, get_ghc_opts, is_haskell_source, show_panel, hide_panel, head_of
    from SublimeHaskell.internals.settings import get_setting_async
    from SublimeHaskell.autocomplete import get_qualified_symbol_at_region
    import SublimeHaskell.hsdev as hsdev
    from SublimeHaskell.hdevtools import hdevtools_type
    from SublimeHaskell.check_lint import ghcmod_type
    from SublimeHaskell.parseoutput import sublime_column_to_ghc_column, ghc_column_to_sublime_column
    from SublimeHaskell.symbols import unicode_operators, use_unicode_operators

# Used to find out the module name.
MODULE_RE_STR = r'module\s+([^\s\(]*)'  # "module" followed by everything that is neither " " nor "("
MODULE_RE = re.compile(MODULE_RE_STR)

# Parses the output of `ghc-mod type`.
# Example: 39 1 40 17 "[Char]"
GHCMOD_TYPE_LINE_RE = re.compile(r'(?P<startrow>\d+) (?P<startcol>\d+) (?P<endrow>\d+) (?P<endcol>\d+) "(?P<type>.*)"')

# Name of the sublime panel in which type information is shown.
TYPES_PANEL_NAME = 'sublime_haskell_show_types_panel'


def parse_ghc_mod_type_line(l):
    """
    Returns the `groupdict()` of GHCMOD_TYPE_LINE_RE matching the given line,
    of `None` if it doesn't match.
    """
    match = GHCMOD_TYPE_LINE_RE.match(l)
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
    def from_point(view, p):
        (l, c) = view.rowcol(p)
        return FilePosition(int(l), int(c))

    @staticmethod
    def from_type_pos(view, l, c):
        return FilePosition(int(l) - 1, ghc_column_to_sublime_column(view, int(l), int(c)))

    # From one-based line-column
    @staticmethod
    def from_str(s):
        if not s:
            return None
        [l, c] = s.split(':')
        return FilePosition(int(l - 1), int(c - 1))


def position_by_point(view, point):
    return FilePosition.from_point(view, point)


class RegionType(object):
    def __init__(self, typename, start, end = None):
        self.typename = typename
        self.start = start
        self.end = end if end else start

    def region(self, view):
        return sublime.Region(self.start.point(view), self.end.point(view))

    def substr(self, view):
        return view.substr(self.region(view))

    @unicode_operators
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

    @unicode_operators
    def show(self, view):
        fmt = '{0} :: {1}' if len(self.expr.splitlines()) == 1 else '{0}\n\t:: {1}'
        return fmt.format(self.expr, self.typename)

    def __eq__(self, r):
        return self.region == r.region

    def contains(self, r):
        return self.contains_region(r.region)

    def contains_region(self, r):
        return self.region.contains(r)

    @staticmethod
    def fromRegionType(r, view):
        return TypedRegion(view, r.region(view), r.typename)


def region_by_region(view, region, typename):
    return RegionType(typename, position_by_point(view, region.a), position_by_point(view, region.b))

TYPE_RE = re.compile(r'(?P<line1>\d+)\s+(?P<col1>\d+)\s+(?P<line2>\d+)\s+(?P<col2>\d+)\s+"(?P<type>.*)"$')


def parse_type_output(view, s):
    result = []
    for l in s.splitlines():
        matched = TYPE_RE.match(l)
        if matched:
            result.append(RegionType(
                matched.group('type'),
                FilePosition.from_type_pos(view, int(matched.group('line1')), int(matched.group('col1'))),
                FilePosition.from_type_pos(view, int(matched.group('line2')), int(matched.group('col2')))))

    return result


def sorted_types(view, types, pt):
    return sorted(
        list(filter(lambda t: t.region(view).contains(pt), types)),
        key = lambda t: t.region(view).size())


def get_type(view, filename, module_name, line, column, cabal = None):
    result = None

    if get_setting_async('enable_hsdev'):
        # Convert from hsdev one-based locations to sublime zero-based positions
        ts = get_types(filename, cabal=cabal) or []
        pt = FilePosition(line, column).point(view)
        return sorted_types(view, ts, pt)
    column = sublime_column_to_ghc_column(view, line, column)
    line = line + 1
    if get_setting_async('enable_hdevtools'):
        result = hdevtools_type(filename, line, column, cabal = cabal)
    if not result and module_name and get_setting_async('enable_ghc_mod'):
        result = ghcmod_type(filename, module_name, line, column)
    return parse_type_output(view, result) if result else None


def get_type_view(view, selection = None):
    filename = view.file_name()

    if selection is None:
        selection = view.sel()[0]

    (r, c) = view.rowcol(selection.b)
    line = r
    column = c

    module_name = None
    m = head_of(hsdev.client.module(file = filename))
    if m:
        module_name = m.name

    return get_type(view, filename, module_name, line, column)


def get_types(filename, on_result = None, cabal = None):
    if get_setting_async('enable_hsdev'):
        def to_file_pos(r):
            return FilePosition(int(r['line']) - 1, int(r['column']) - 1)

        def to_region_type(r):
            return RegionType(
                r['note']['type'],
                to_file_pos(r['region']['from']),
                to_file_pos(r['region']['to']))

        def on_resp(rs):
            ts = [to_region_type(r) for r in rs]
            file_types.set(filename, ts, False)
            on_result(ts)

        if file_types.has(filename):
            return file_types.get(filename)

        wait = on_result is None
        res = hsdev.client.types(files = [filename], ghc = get_ghc_opts(filename), wait = wait, on_response = on_resp if on_result is not None else None)
        if res is not None and wait:
            ts = [to_region_type(r) for r in res]
            file_types.set(filename, ts, False)
            return ts


class SublimeHaskellShowType(SublimeHaskellTextCommand):
    def run(self, edit, filename = None, line = None, column = None):
        result = self.get_types(filename, int(line) if line else None, int(column) if column else None)
        self.show_types(result)

    def get_types(self, filename = None, line = None, column = None):
        if not filename:
            filename = self.view.file_name()

        if (not line) or (not column):
            (r, c) = self.view.rowcol(self.view.sel()[0].b)
            line = r
            column = c

        module_name = None
        m = head_of(hsdev.client.module(file = filename))
        if m:
            module_name = m.name

        return get_type(self.view, filename, module_name, line, column)

    def get_best_type(self, types):
        if not types:
            return None

        region = self.view.sel()[0]
        file_region = region_by_region(self.view, region, '')
        if region.a != region.b:
            return sorted(types, key = lambda r: file_region.precise_in_region(self.view, r))[0]
        else:
            return types[0]

    def show_types(self, types):
        if not types:
            show_status_message("Can't infer type", False)
            return

        self.types = types
        self.output_view = output_panel(self.view.window(), '', panel_name = TYPES_PANEL_NAME, syntax = 'Haskell-SublimeHaskell')
        self.view.window().show_quick_panel([t.typename for t in self.types], self.on_done, 0, -1, self.on_changed)

    def on_done(self, idx):
        self.view.erase_regions('typed')

        if idx == -1:
            return

        t = self.types[idx]
        output_text(self.output_view, t.show(self.view), clear = True)

    def on_changed(self, idx):
        if idx == -1:
            return

        t = self.types[idx]
        output_text(self.output_view, t.show(self.view), clear = True)
        self.view.add_regions('typed', [t.region(self.view)], 'string', 'dot', sublime.DRAW_OUTLINED)

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)


class FileTypes(object):
    def __init__(self):
        self.types = {}
        self.status = {}

    def set(self, filename, types, show = True):
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

file_types = FileTypes()


class SublimeHaskellShowTypes(SublimeHaskellShowType):
    def run(self, edit, filename = None, line = None, column = None):
        result = self.get_types(filename, int(line) if line else None, int(column) if column else None)
        self.show_types(result)

    def show_types(self, types):
        if not types:
            show_status_message("Can't infer type", False)
            return

        self.types = types
        self.output_view = output_panel(self.view.window(), '', panel_name = TYPES_PANEL_NAME, syntax = 'Haskell-SublimeHaskell', show_panel = False)
        regions = []
        for t in self.types:
            output_text(self.output_view, '{0}\n'.format(t.show(self.view)), clear = False)
            regions.append(sublime.Region(self.output_view.size() - 1 - len(use_unicode_operators(t.typename)), self.output_view.size() - 1))
        self.output_view.add_regions('types', regions, 'comment', '', sublime.DRAW_OUTLINED)
        show_panel(self.view.window(), panel_name = TYPES_PANEL_NAME)


class SublimeHaskellGetTypes(SublimeHaskellTextCommand):
    def run(self, edit, filename = None):
        self.filename = filename
        if not self.filename:
            self.filename = self.view.file_name()
        if not file_types.has(self.filename):
            get_types(self.filename, self.on_types)

    def on_types(self, types):
        pass


class SublimeHaskellShowAllTypes(SublimeHaskellTextCommand):
    def run(self, edit, filename = None):
        self.filename = filename
        if not self.filename:
            self.filename = self.view.file_name()
        if not file_types.has(self.filename):
            get_types(self.filename, self.on_types)
        else:
            self.on_types(file_types.get(self.filename))

    def on_types(self, types):
        file_types.show(self.filename)
        self.show_types(types)

    def show_types(self, types):
        if not types:
            show_status_message("Can't infer type", False)
            return

        types = sorted(
            list(filter(lambda t: t.region(self.view).contains(self.view.sel()[0]), types)),
            key = lambda t: t.region(self.view).size())
        self.output_view = output_panel(self.view.window(), '', panel_name = TYPES_PANEL_NAME, syntax = 'Haskell-SublimeHaskell', show_panel = False)

        regions = []
        for t in types:
            output_text(self.output_view, '{0}\n'.format(t.show(self.view)), clear = False)
            regions.append(sublime.Region(self.output_view.size() - 1 - len(use_unicode_operators(t.typename)), self.output_view.size() - 1))
        self.output_view.add_regions('types', regions, 'comment', '', sublime.DRAW_OUTLINED)
        show_panel(self.view.window(), panel_name = TYPES_PANEL_NAME)

    def is_enabled(self):
        return is_haskell_source(self.view) and self.view.file_name() is not None


class SublimeHaskellHideAllTypes(SublimeHaskellTextCommand):
    def run(self, edit):
        file_types.hide(self.view.file_name())
        hide_panel(self.view.window(), panel_name = TYPES_PANEL_NAME)

    def is_enabled(self):
        return is_haskell_source(self.view) and self.view.file_name() is not None and file_types.has(self.view.file_name()) and file_types.shown(self.view.file_name())


class SublimeHaskellToggleAllTypes(SublimeHaskellTextCommand):
    def run(self, edit):
        if file_types.shown(self.view.file_name()):
            self.view.run_command('sublime_haskell_hide_all_types')
        else:
            self.view.run_command('sublime_haskell_show_all_types')

    def is_enabled(self):
        return is_haskell_source(self.view) and self.view.file_name() is not None


# Works only with the cursor being in the name of a toplevel function so far.
class SublimeHaskellInsertType(SublimeHaskellShowType):
    def run(self, edit):
        result = self.get_best_type(self.get_types())
        if result:
            r = result.region(self.view)
            qsymbol = get_qualified_symbol_at_region(self.view, self.view.word(r.begin()))
            line_begin = self.view.line(r).begin()
            prefix = self.view.substr(sublime.Region(line_begin, r.begin()))
            indent = re.search('(?P<indent>\s*)', prefix).group('indent')
            signature = '{0}{1} :: {2}\n'.format(indent, qsymbol.name, result.typename)
            self.view.insert(edit, line_begin, signature)


class ExpandSelectionInfo(object):
    def __init__(self, view, selection = None):
        self.view = view
        self.selection = selection if selection is not None else view.sel()[0]
        if file_types.has(self.view.file_name()):
            types = sorted_types(self.view, file_types.get(self.view.file_name()), self.selection.b)
        else:
            types = get_type_view(self.view, self.selection)
        self.regions = [TypedRegion.fromRegionType(t, view) for t in types] if types else None
        self.expanded_index = None

    def is_valid(self):
        return self.regions is not None

    def is_actual(self, view = None, selection = None):
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
            for i, r in enumerate(self.regions):
                if r.contains_region(self.selection) and r.region != self.selection:
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

    def run(self, edit):
        selections = list(self.view.sel())

        if not self.is_infos_valid(selections):
            self.Infos = [ExpandSelectionInfo(self.view, s) for s in selections]

        if not self.is_infos_valid(selections):
            show_status_message('Unable to retrieve expand selection info', False)
            return

        tr = [i.expand() for i in self.Infos]
        self.view.sel().clear()
        self.view.sel().add_all([t.region for t in tr])

        output_panel(self.view.window(), '\n'.join([use_unicode_operators(t.typename) for t in tr]), panel_name = 'sublime_haskell_expand_selection_expression', syntax = 'Haskell-SublimeHaskell')

    def is_infos_valid(self, selections):
        return self.Infos and all([i.is_valid() for i in self.Infos]) and len(selections) == len(self.Infos) and all([i.is_actual(self.view, s) for i, s in zip(self.Infos, selections)])


class SublimeHaskellTypes(sublime_plugin.EventListener):
    def on_selection_modified(self, view):
        if is_haskell_source(view) and view.file_name() and file_types.has(view.file_name()) and file_types.shown(view.file_name()):
            view.run_command('sublime_haskell_show_all_types', {'filename': view.file_name()})

    def on_modified(self, view):
        file_types.remove(view.file_name())
