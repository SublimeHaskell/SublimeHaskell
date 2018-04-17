import urllib.parse

import webbrowser
import json
from xml.etree import ElementTree

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.internals.unicode_opers as UnicodeOpers
import SublimeHaskell.symbols as symbols
import SublimeHaskell.internals.backend_mgr as BackendManager
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.parseoutput as ParseOutput


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

    def load_scheme(self, scheme_path):
        if scheme_path not in self.schemes:
            scheme_res = sublime.load_resource(scheme_path)
            if scheme_res:
                # Go through all styles and collect scope/foreground/fontStyle etc.
                # Prefer ST3 'sublime-color-scheme' JSON over older TextMate XML.
                self.schemes[scheme_path] = self.collect_sublime_scheme(json.loads(scheme_res)) \
                    if scheme_path.endswith('.sublime-color-scheme') \
                    else self.collect_textmate_scheme(ElementTree.fromstring(scheme_res))

        return self.schemes.get(scheme_path, {})

    def collect_textmate_scheme(self, scheme_tree):
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

    def collect_sublime_scheme(self, scheme_dict):
        scheme = {}
        for rule in scheme_dict.get('rules', []):
            scope = rule.get('scope', '')
            if scope:
                scheme[scope] = rule

        return scheme

    def gen_style(self, scheme_path):
        scheme = self.load_scheme(scheme_path)

        parts = []
        parts.append("<style>")
        parts.append("a { text-decoration: underline; }")
        # generate CSS style for each class
        for cls, scope in self.CSS_CLASSES.items():
            # find scope or its parent in scheme
            scope_parts = scope.split('.')
            for css_scope in reversed(['.'.join(scope_parts[0:i+1]) for i in range(0, len(scope_parts))]):
                if css_scope in scheme:  # Found some scope, fill style class
                    style_parts = []
                    if 'foreground' in scheme[css_scope]:
                        style_parts.append("color: {0}".format(scheme[css_scope]['foreground']))
                    # Prefer ST3 'sublime-color-scheme' JSON attribute over the older TextMate-ish name
                    font_style = scheme[css_scope].get('font_style', scheme[css_scope].get('fontStyle', ''))
                    if font_style:
                        style_parts.append("font-style: {0}".format(font_style))
                    parts.append(".{0} {{ {1} }}".format(cls, "; ".join(style_parts)))
                    break
        parts.append("</style>")
        return "".join(parts)


class SublimeHaskellHoverPopup(object):
    # HTML style formatting
    STYLES = Styles()

    def __init__(self, view, filename, point, hover_zone):
        super().__init__()
        self.view = view
        self.filename = filename
        self.point = point
        self.hover_zone = hover_zone
        self.line, self.column = view.rowcol(point)
        self.shown = False


    def do_hover(self):
        if self.hover_zone == sublime.HOVER_TEXT:
            qsymbol = Common.get_qualified_symbol_at_point(self.view, self.point)
            ## print('hover: qualified symbol {0}'.format(qsymbol))
            module_word = qsymbol.module
            ident = qsymbol.name
            project_dir = Common.locate_cabal_project_from_view(self.view)[0]

            if module_word is not None and ident is None:
                # TODO: Any ideas for popup about module?
                pass
            elif ident is not None:
                whois_name = qsymbol.qualified_name()
                full_name = qsymbol.full_name()

                # Infer types in backgroup (if not any)
                if Settings.PLUGIN.enable_infer_types:
                    BackendManager.active_backend().infer(files=[self.filename])

                # Try get type of hovered symbol
                typed_expr = None
                # Getting type is slow, disabled for me

                # if types.SourceHaskellTypeCache().has(self.filename):
                #     typed_expr = self.get_type(types.SourceHaskellTypeCache().get(self.filename), whois_name)
                # else:
                #     project_name = Common.locate_cabal_project_from_view(self.view)[1]
                #     point_rgn = sublime.Region(self.point, self.point)
                #     typed_expr = self.get_type(types.get_type_view(self.view, project_name, point_rgn), whois_name)

                # Try whois
                suggest_import = False
                whoat = BackendManager.active_backend().whoat(self.line + 1, self.column + 1, self.filename)
                decl = Utils.head_of(whoat)
                usages = BackendManager.active_backend().usages(self.line + 1, self.column + 1, self.filename) if decl else None
                if usages:
                    usages = [
                        u for u in usages
                        if not u.definition_usage() and (u.used_in.location.filename == self.filename or u.used_in.location.project_path() == project_dir)
                    ]
                if not decl:
                    decl = Utils.head_of(BackendManager.active_backend().whois(whois_name, self.filename))
                if not decl:
                    suggest_import = True
                    decl = Utils.head_of(BackendManager.active_backend().lookup(full_name, self.filename))

                self.create_symbol_popup(typed_expr, decl, suggest_import, usages=usages)

        elif self.hover_zone == sublime.HOVER_GUTTER:
            errs = [err for err in ParseOutput.MARKER_MANAGER.marks_for_view(self.view) if err.region.start.line == self.line]
            if errs:
                popup_parts = [self.STYLES.gen_style(self.view.settings().get('color_scheme'))]
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
                self.shown = True
                self.view.show_popup(popup_text, sublime.HIDE_ON_MOUSE_MOVE_AWAY, self.point, 600, 600,
                                     self.on_navigate, self.on_hide)


    def create_symbol_popup(self, typed_expr, decl, suggest_import, usages=None):
        if typed_expr or decl:
            popup_parts = [self.STYLES.gen_style(self.view.settings().get('color_scheme'))]
            if typed_expr:
                popup_parts.append(u'<p><span class="function">{0}</span>{1}</p>'.format(
                    typed_expr.substr(self.view),
                    symbols.format_type(UnicodeOpers.use_unicode_operators(' :: {0}'.format(typed_expr.typename)))))
            if decl:
                popup_msg = [u'<a href="import:{0}">Add import</a>'.format(urllib.parse.quote_plus(decl.name))] \
                            if suggest_import else []
                popup_parts.append(decl.popup(popup_msg))
            if usages is not None:
                source_symbol = decl.by_source()
                used_total = len(usages)
                used_here = len([u for u in usages if u.used_in.location.filename == self.filename])
                used_defm = len([u for u in usages if u.internal_usage()])

                usages_ref = '<a href="usages:{0}:{1}">Usages</a>'.format(self.line, self.column)

                if used_total == 0:
                    usages_tpl = 'Not used'
                elif not source_symbol:
                    usages_tpl = '{usages_ref}: {total} (<a href="select:{line}:{column}">{here} in this file</a>)'
                else:
                    if decl.module.location.filename == self.filename:
                        usages_tpl = '{usages_ref}: {total} (<a href="select:{line}:{column}">{here} in this file</a>)'
                    else:
                        usages_tpl = '{usages_ref}: {total} (<a href="select:{line}:{column}">{here} in this file</a>, {defm} in def file)'
                usages_msg = usages_tpl.format(
                    usages_ref=usages_ref,
                    total=used_total,
                    here=used_here,
                    defm=used_defm,
                    line=self.line,
                    column=self.column,
                )

                popup_parts.append(u'<span class="comment">{0}</span>'.format(usages_msg))

            popup_text = u''.join(popup_parts)
            if not self.shown:
                self.shown = True
                self.view.show_popup(popup_text, sublime.HIDE_ON_MOUSE_MOVE_AWAY, self.point, 600, 600,
                                     self.on_navigate, self.on_hide)
            else:
                self.view.update_popup(popup_text)


    def get_type(self, type_list, qual_name):
        filt_types = [t for t in type_list
                      if t.substr(self.view) == qual_name and t.region(self.view).contains(self.point)]
        return Utils.head_of(filt_types)


    def on_navigate(self, url):
        if self.view.is_popup_visible():
            self.view.hide_popup()
            if url[0:4] == 'http':
                webbrowser.open(url)
            elif url[0:8] == 'autofix:':
                rgn = symbols.Region.from_str(url[8:])
                ParseOutput.MARKER_MANAGER.apply_autocorrect(self.view, rgn)
            elif url[0:7] == "import:":
                decl_name = urllib.parse.unquote(url[7:])
                self.view.run_command('sublime_haskell_insert_import_for_symbol',
                                      {'filename': self.view.file_name(),
                                       'decl': decl_name})
            elif url[0:7] == "usages:":
                line, column = tuple(map(int, url.split(':')[1:]))
                self.view.run_command(
                    'sublime_haskell_symbol_usages',
                    {
                        'filename': self.view.file_name(),
                        'line': line,
                        'column': column,
                    }
                )
            elif url[0:7] == "select:":
                line, column = tuple(map(int, url.split(':')[1:]))
                self.view.run_command(
                    'sublime_haskell_select_symbol_occurrences',
                    {
                        'line': line,
                        'column': column,
                    }
                )
            else:
                self.view.window().open_file(url, sublime.ENCODED_POSITION)

    def on_hide(self):
        self.shown = False
