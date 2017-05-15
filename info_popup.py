import html
import webbrowser
from xml.etree import ElementTree

import sublime
import sublime_plugin

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.internals.unicode_opers as UnicodeOpers
import SublimeHaskell.symbols as symbols
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.parseoutput as parseoutput
import SublimeHaskell.types as types


CSS_CLASSES = {
    'comment': 'comment',
    'function': 'entity.name.function',
    'type': 'entity.name.type',
    'operator': 'keyword.operator',
    'keyword': 'keyword.declaration',
    'tyvar': 'variable.generic',
    'error': 'sublimehaskell.mark.error',
    'warning': 'sublimehaskell.mark.warning',
    'hint': 'sublimehaskell.mark.hint'
}


# Unused module variable:
# style_header = "<style>" \
#     "a { text-decoration: underline; }" \
#     ".type { color: red; }" \
#     ".tyvar { color: blue; }" \
#     ".operator { color: green; }" \
#     ".comment { color: gray; font-style: italic; }" \
#     ".docs { color: gray; }" \
#     "</style>"


class Styles(object):
    """
    Loads and holds cache of scheme styles
    Also generates style header
    """
    def __init__(self):
        self.schemes = {}

    def load_scheme(self, scheme_path):
        if scheme_path not in self.schemes:
            scheme_res = sublime.load_resource(scheme_path)
            if scheme_res:
                # Go through all styles and collect scope/foreground/fontStyle etc.
                self.schemes[scheme_path] = self.collect_scheme(ElementTree.fromstring(scheme_res))

        return self.schemes.get(scheme_path, {})

    def collect_scheme(self, scheme_tree):
        scheme = {}
        for style in scheme_tree.findall(".//dict[key='scope']"):
            try:
                cur_style = {}
                cur_tag = None
                for elem in style.iter():
                    if elem.tag == 'key':
                        cur_tag = elem.text  # We are going to fill it next time
                    elif elem.tag == 'string' and cur_tag is not None:
                        cur_style[cur_tag] = elem.text
                        cur_tag = None
                if 'scope' in cur_style:
                    scheme[cur_style['scope']] = cur_style
            except ValueError:
                pass
        return scheme

    def gen_style(self, scheme_path):
        scheme = self.load_scheme(scheme_path)

        parts = []
        parts.append("<style>")
        parts.append("a { text-decoration: underline; }")
        # generate CSS style for each class
        for cls, scope in CSS_CLASSES.items():
            # find scope or its parent in scheme
            scope_parts = scope.split('.')
            scopes = ['.'.join(scope_parts[0:i+1]) for i in range(0, len(scope_parts))]
            for scope in reversed(scopes):
                if scope in scheme:  # Found some scope, fill style class
                    style_parts = []
                    if 'foreground' in scheme[scope]:
                        style_parts.append("color: {0}".format(scheme[scope]['foreground']))
                    if 'fontStyle' in scheme[scope]:
                        style_parts.append("font-style: {0}".format(scheme[scope]['fontStyle']))
                    parts.append(".{0} {{ {1} }}".format(cls, "; ".join(style_parts)))
                    break
        parts.append("</style>")
        return "".join(parts)

SUBHASK_STYLES = Styles()


class SublimeHaskellPopup(sublime_plugin.EventListener):
    def __init__(self):
        super().__init__()
        self.view = None
        self.current_file_name = None
        self.point = None
        self.decl = None
        self.typed_expr = None
        self.whois_name = None
        self.full_name = None
        self.suggest_import = None

    def on_hover(self, view, point, hover_zone):
        # Note: view.file_name() is not set in certain views, such as the "Haskell Show Types Panel". Avoid
        # generating lookup errors, which are logged in the console window (for better or worse.)
        if Common.is_haskell_source(view) and view.file_name():
            # Ensure that we never block the Python main thread.
            Utils.run_async('SublimeHaskellPopup.on_hover', self.do_hover, view, point, hover_zone)

    def do_hover(self, view, point, hover_zone):
        self.view = view
        self.current_file_name = view.file_name()
        # If the column is needed: (line, column) = self.view.rowcol(point) [remove subscript]
        line = view.rowcol(point)[0]
        self.decl = None
        self.typed_expr = None

        if hover_zone == sublime.HOVER_TEXT:
            qsymbol = Common.get_qualified_symbol_at_point(view, point)
            module_word = qsymbol.module
            ident = qsymbol.name

            if ident is None and module_word:  # TODO: Any ideas for popup about module?
                pass

            if ident:
                self.whois_name = qsymbol.qualified_name()
                self.full_name = qsymbol.full_name()

                # Try get type of hovered symbol
                self.point = point
                self.typed_expr = None
                if types.SourceHaskellTypeCache().has(self.current_file_name):
                    self.typed_expr = self.get_type(types.SourceHaskellTypeCache().get(self.current_file_name))
                else:
                    type_list = types.query_file_types(self.current_file_name)
                    self.on_types(type_list)

                # Try whois
                self.suggest_import = False
                self.decl = Utils.head_of(BackendManager.active_backend().whois(self.whois_name, self.current_file_name))
                if not self.decl:
                    self.suggest_import = True
                    self.decl = Utils.head_of(BackendManager.active_backend().lookup(self.full_name, self.current_file_name))

                self.create_symbol_popup(update=True)

        elif hover_zone == sublime.HOVER_GUTTER:
            self.view = view
            self.current_file_name = self.view.file_name()
            errs = list(filter(lambda e: e.region.start.line == line, parseoutput.errors_for_view(self.view)))
            if errs:
                popup_parts = [SUBHASK_STYLES.gen_style(self.view.settings().get('color_scheme'))]
                for err in errs:
                    msg = UnicodeOpers.use_unicode_operators(symbols.escape_text(err.message))
                    # Decorate first word with style
                    decors = {
                        'Error': 'error',
                        'Warning': 'warning',
                        'Hint': 'hint'
                    }
                    for dec, dec_style in decors.items():
                        msg = msg.replace(dec, u'<span class="{0}">{1}</span>'.format(dec_style, dec))
                    popup_parts.append(u'<p>{0}</p>'.format(msg))
                    if err.correction is not None:
                        popup_parts.append(err.correction.popup())
                popup_text = u''.join(popup_parts)
                self.view.show_popup(popup_text, sublime.HIDE_ON_MOUSE_MOVE_AWAY, point, 600, 600,
                                     self.on_navigate, self.on_hide)

    def create_symbol_popup(self, update=False):
        if self.typed_expr or self.decl:
            popup_parts = [SUBHASK_STYLES.gen_style(self.view.settings().get('color_scheme'))]
            if self.typed_expr:
                popup_parts.append(u'<p><span class="function">{0}</span>{1}</p>'.format(
                    self.typed_expr.substr(self.view),
                    symbols.format_type(UnicodeOpers.use_unicode_operators(u' :: {0}'.format(self.typed_expr.typename)))))
            if self.decl:
                popup_msg = [u'<a href="import:{0}">Add import</a>'.format(html.escape(self.decl.name))] \
                            if self.suggest_import \
                            else []
                popup_parts.append(self.decl.popup(popup_msg))
            popup_text = u''.join(popup_parts)
            if update and self.view.is_popup_visible():
                self.view.update_popup(popup_text)
            else:
                self.view.show_popup(popup_text, sublime.HIDE_ON_MOUSE_MOVE_AWAY, self.point, 600, 600,
                                     self.on_navigate, self.on_hide)

    def get_type(self, type_list):
        filt_types = [t for t in type_list
                      if t.substr(self.view) == self.whois_name and t.region(self.view).contains(self.point)]
        return Utils.head_of(filt_types)

    def on_types(self, type_list):
        self.typed_expr = self.get_type(type_list)
        if self.typed_expr:
            self.create_symbol_popup(update=True)

    def on_navigate(self, url):
        if self.view.is_popup_visible():
            self.view.hide_popup()
            if url[0:4] == 'http':
                webbrowser.open(url)
            elif url[0:8] == 'autofix:':
                rgn = symbols.Region.from_str(url[8:])
                errs = parseoutput.errors_for_view(self.view)
                # Give err_text, err_rgn scope other than the loop.
                err_text = ''
                err_rgn = None

                for err in errs:
                    if err.correction is not None and err.correction.corrector.region == rgn:
                        err.erase_from_view()
                        parseoutput.ERRORS.remove(err)
                        errs.remove(err)
                        parseoutput.update_messages_in_view(self.view, errs)

                        corrector = err.correction.corrector
                        err_rgn = corrector.to_region(self.view)
                        err_text = corrector.contents

                        sublime.set_timeout(lambda: self.view.run_command('sublime_haskell_replace_text',
                                                                          {'text': err_text,
                                                                           'begin': err_rgn.begin(),
                                                                           'end': err_rgn.end()}),
                                            0)
                        return
            elif url[0:7] == "import:":
                self.view.run_command('sublime_haskell_insert_import_for_symbol',
                                      {'filename': self.view.file_name(),
                                       'decl': self.decl.name})
            else:
                self.view.window().open_file(url, sublime.ENCODED_POSITION | sublime.TRANSIENT)

    def on_hide(self):
        pass
