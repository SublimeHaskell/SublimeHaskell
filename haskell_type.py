import sublime
import sublime_plugin
import re

from sublime_haskell_common import call_ghcmod_and_wait

# Used to find out the module name.
MODULE_RE_STR = r'module\s+([^\s\(]*)' # "module" followed by everything that is neither " " nor "("
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


class HaskellShowTypeCommand(sublime_plugin.TextCommand):
    def ghcmod_get_type_of_cursor(self):
        view = self.view

        filename = str(view.file_name())
        row, col = view.rowcol(view.sel()[0].a)
        row1, col1 = row + 1, col + 1  # ghc-mod uses rows/cols starting with 1
        module_region = view.find(MODULE_RE_STR, 0)

        if module_region is None:
            sublime.status_message("SublimeHaskell: Could not determine module name!")
            return None

        # RE must match; there is only one group in the RE.
        module = MODULE_RE.match(view.substr(module_region)).group(1)

        ghcmod_args = ['type', filename, module, str(row1), str(col1)]
        out = call_ghcmod_and_wait(ghcmod_args)

        if not out:
            sublime.status_message("ghc-mod %s returned nothing" % ' '.join(ghcmod_args))
            return None

        # ghc-mod type returns the type of the expression at at the given row/col.
        # It can return multiple lines, extending the expression scope by one level each.
        # The last line belongs to the toplevel expression.
        types = map(parse_ghc_mod_type_line, out.strip().split('\n'))
        result_type = types[0]['type']  # innermost expression's type

        if not result_type:
            sublime.error_message("ghc-mod type returned unexpected output")
            return None

        return result_type

    def run(self, edit):
        result_type = self.ghcmod_get_type_of_cursor()

        if result_type:
            self.write_output(self.view, result_type)

    def write_output(self, view, text):
        "Write text to Sublime's output panel."
        output_view = view.window().get_output_panel(TYPE_PANEL_NAME)
        output_view.set_read_only(False)
        # Write to the output buffer:
        edit = output_view.begin_edit()
        output_view.insert(edit, 0, text)
        output_view.end_edit(edit)
        # Set the selection to the beginning of the view so that "next result" works:
        output_view.set_read_only(True)
        # Show the results panel:
        view.window().run_command('show_panel', {'panel': 'output.' + TYPE_PANEL_NAME})


# Works only with the cursor being in the name of a toplevel function so far.
class HaskellInsertTypeCommand(HaskellShowTypeCommand):
    def run(self, edit):
        view = self.view
        result_type = self.ghcmod_get_type_of_cursor()

        if result_type:
            # TODO get this from ghc-mod as well, e.g. from the range of the type
            word_region = view.word(view.sel()[0])
            line_region = view.line(view.sel()[0])
            indent_region = sublime.Region(line_region.begin(), word_region.begin())

            indent = view.substr(indent_region)
            fn_name = view.substr(word_region)

            signature = "{0}{1} :: {2}\n".format(indent, fn_name, result_type)

            view.insert(edit, line_region.begin(), signature)
