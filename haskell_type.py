import sublime
import sublime_plugin
import re

if int(sublime.version()) < 3000:
    from sublime_haskell_common import is_enabled_haskell_command, get_setting_async, show_status_message
    from autocomplete import autocompletion
    from hdevtools import hdevtools_type, hdevtools_enabled
    from ghcmod import ghcmod_type, ghcmod_enabled
else:
    from SublimeHaskell.sublime_haskell_common import is_enabled_haskell_command, get_setting_async, show_status_message
    from SublimeHaskell.autocomplete import autocompletion
    from SublimeHaskell.hdevtools import hdevtools_type, hdevtools_enabled
    from SublimeHaskell.ghcmod import ghcmod_type, ghcmod_enabled
    from functools import reduce

# Used to find out the module name.
MODULE_RE_STR = r'module\s+([^\s\(]*)'  # "module" followed by everything that is neither " " nor "("
MODULE_RE = re.compile(MODULE_RE_STR)

# Parses the output of `ghc-mod type`.
# Example: 39 1 40 17 "[Char]"
GHCMOD_TYPE_LINE_RE = re.compile(r'(?P<startrow>\d+) (?P<startcol>\d+) (?P<endrow>\d+) (?P<endcol>\d+) "(?P<type>.*)"')

# Name of the sublime panel in which type information is shown.
TYPE_PANEL_NAME = 'haskell_type_panel'

def parse_ghc_mod_type_line(l):
    """
    Returns the `groupdict()` of GHCMOD_TYPE_LINE_RE matching the given line,
    of `None` if it doesn't match.
    """
    match = GHCMOD_TYPE_LINE_RE.match(l)
    return match and match.groupdict()

def tabs_offset(view, point):
    """
    Returns count of '\t' before point in line multiplied by 7
    8 is size of type as supposed by ghc-mod, to every '\t' will add 7 to column
    Subtract this value to get sublime column by ghc-mod column, add to get ghc-mod column by sublime column
    """
    cur_line = view.substr(view.line(point))
    return len(list(filter(lambda ch: ch == '\t', cur_line))) * 7

def sublime_column_to_type_column(view, line, column):
    cur_line = view.substr(view.line(view.text_point(line, column)))
    return column + len(list(filter(lambda ch: ch == '\t', cur_line))) * 7 + 1

def type_column_to_sublime_column(view, line, column):
    cur_line = view.substr(view.line(view.text_point(line - 1, 0)))
    col = 1
    real_col = 0
    for c in cur_line:
        if col >= column:
            return real_col
        col += (8 if c == '\t' else 1)
        real_col += 1
    return real_col

class FilePosition(object):
    def __init__(self, line, column):
        self.line = line
        self.column = column

    def point(self, view):
        # Note, that sublime suppose that '\t' is 'tab_size' length
        # But '\t' is one character
        return view.text_point(self.line - 1, type_column_to_sublime_column(view, self.line, self.column))

def position_by_point(view, point):
    tabs = tabs_offset(view, point)
    (r, c) = view.rowcol(point)
    return FilePosition(r + 1, c + 1 + tabs)

class RegionType(object):
    def __init__(self, typename, start, end = None):
        self.typename = typename
        self.start = start
        self.end = end if end else start

    def region(self, view):
        return sublime.Region(self.start.point(view), self.end.point(view))

    def substr(self, view):
        return view.substr(self.region(view))

    def show(self, view):
        return '{0} :: {1}'.format(self.substr(view), self.typename)

    def precise_in_region(self, view, other):
        this_region = self.region(view)
        other_region = other.region(view)
        if other_region.contains(this_region):
            return (0, other_region.size() - this_region.size())
        elif other_region.intersects(this_region):
            return (1, -other_region.intersection(this_region).size())
        return (2, 0)

def region_by_region(view, region, typename):
    return RegionType(typename, position_by_point(view, region.a), position_by_point(view, region.b))

TYPE_RE = re.compile(r'(?P<line1>\d+)\s+(?P<col1>\d+)\s+(?P<line2>\d+)\s+(?P<col2>\d+)\s+"(?P<type>.*)"$')

def parse_type_output(s):
    result = []
    for l in s.splitlines():
        matched = TYPE_RE.match(l)
        if matched:
            result.append(RegionType(
                matched.group('type'),
                FilePosition(int(matched.group('line1')), int(matched.group('col1'))),
                FilePosition(int(matched.group('line2')), int(matched.group('col2')))))

    return result

def haskell_type(filename, module_name, line, column, cabal = None):
    result = None

    if hdevtools_enabled():
        result = hdevtools_type(filename, line, column, cabal = cabal)
    if not result and module_name and ghcmod_enabled():
        result = ghcmod_type(filename, module_name, line, column, cabal = cabal)
    return parse_type_output(result) if result else None

class SublimeHaskellShowType(sublime_plugin.TextCommand):
    def run(self, edit, filename = None, line = None, column = None):
        result = self.get_types(filename, int(line) if line else None, int(column) if column else None)
        self.show_types(result)

    def get_types(self, filename = None, line = None, column = None):
        if not filename:
            filename = self.view.file_name()

        if (not line) or (not column):
            (r, c) = self.view.rowcol(self.view.sel()[0].b)
            line = r + 1
            column = c + 1

        column = sublime_column_to_type_column(self.view, r, c)

        module_name = None
        with autocompletion.database.files as files:
            if filename in files:
                module_name = files[filename].name

        return haskell_type(filename, module_name, line, column)

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

        best_result = self.get_best_type(types)

        type_text = [best_result.show(self.view), '']
        type_text.extend([r.show(self.view) for r in types if r.start.line == r.end.line])

        output_view = self.view.window().get_output_panel('sublime_haskell_hdevtools_type')
        output_view.set_read_only(False)

        output_view.run_command('sublime_haskell_output_text', {
            'text': '\n'.join(type_text) })

        output_view.sel().clear()
        output_view.set_read_only(True)

        self.view.window().run_command('show_panel', {
            'panel': 'output.sublime_haskell_hdevtools_type' })

    def is_enabled(self):
        return is_enabled_haskell_command(self.view, False)


# Works only with the cursor being in the name of a toplevel function so far.
class SublimeHaskellInsertType(SublimeHaskellShowType):
    def run(self, edit):
        result = self.get_best_type(self.get_types())
        if result:
            r = result.region(self.view)
            name = self.view.substr(self.view.word(r.begin()))
            line_begin = self.view.line(r).begin()
            prefix = self.view.substr(sublime.Region(line_begin, r.begin()))
            indent = re.search('(?P<indent>\s*)', prefix).group('indent')
            signature = '{0}{1} :: {2}\n'.format(indent, name, result.typename)
            self.view.insert(edit, line_begin, signature)
