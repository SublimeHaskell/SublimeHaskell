import html
import sublime
import sublime_plugin
import webbrowser
from xml.etree import ElementTree

if int(sublime.version()) < 3000:
	from sublime_haskell_common import *
	import symbols
	import hsdev
	import parseoutput
else:
	from SublimeHaskell.sublime_haskell_common import *
	import SublimeHaskell.symbols as symbols
	import SublimeHaskell.hsdev as hsdev
	import SublimeHaskell.parseoutput as parseoutput


classes = {
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


style_header = "<style>" \
	"a { text-decoration: underline; }" \
	".type { color: red; }" \
	".tyvar { color: blue; }" \
	".operator { color: green; }" \
	".comment { color: gray; font-style: italic; }" \
	".docs { color: gray; }" \
	"</style>"


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
				try:
					# Go through all styles and collect scope/foreground/fontStyle etc.
					scheme_tree = ElementTree.fromstring(scheme_res)
					scheme = {}

					for d in scheme_tree.findall(".//dict[key='scope']"):
						cur_style = {}
						cur_tag = None
						for elem in d.iter():
							if elem.tag == 'key':
								cur_tag = elem.text  # We are going to fill it next time
							elif elem.tag == 'string' and cur_tag is not None:
								cur_style[cur_tag] = elem.text
								cur_tag = None
						if 'scope' in cur_style:
							scheme[cur_style['scope']] = cur_style

					self.schemes[scheme_path] = scheme
				except:
					pass

		return self.schemes.get(scheme_path, {})

	def gen_style(self, scheme_path):
		scheme = self.load_scheme(scheme_path)

		parts = []
		parts.append("<style>")
		parts.append("a { text-decoration: underline; }")
		# generate CSS style for each class
		for cls, scope in classes.items():
			# find scope or its parent in scheme
			scope_parts = scope.split('.')
			scopes = ['.'.join(scope_parts[0:i+1]) for i in range(0, len(scope_parts))]
			for s in reversed(scopes):
				if s in scheme:  # Found some scope, fill style class
					style_parts = []
					if 'foreground' in scheme[s]:
						style_parts.append("color: {0}".format(scheme[s]['foreground']))
					if 'fontStyle' in scheme[s]:
						style_parts.append("font-style: {0}".format(scheme[s]['fontStyle']))
					parts.append(".{0} {{ {1} }}".format(cls, "; ".join(style_parts)))
					break
		parts.append("</style>")
		return "".join(parts)

styles = Styles()


class SublimeHaskellPopup(sublime_plugin.EventListener):
	def on_hover(self, view, point, hover_zone):
		if not is_haskell_source(view):
			return

		if hover_zone == sublime.HOVER_TEXT:
			self.view = view
			self.current_file_name = self.view.file_name()
			qsymbol = get_qualified_symbol_at_point(self.view, point)
			module_word = qsymbol.module
			ident = qsymbol.name

			if ident is None and module_word:  # TODO: Any ideas for popup about module?
				pass

			if ident:
				self.whois_name = qsymbol.qualified_name()
				self.full_name = qsymbol.full_name()

				# Try get type of hovered symbol
				(line, column) = self.view.rowcol(point)
				typed_expr = None
				# typed_expr = head_of(list(filter(
				# 	lambda t:
				# 		(int(t['region']['from']['line']) - 1 == line) and
				# 		(int(t['region']['to']['line']) - 1 == line) and
				# 		(int(t['region']['from']['column']) - 1 <= column) and
				# 		(int(t['region']['to']['column']) - 1 >= column) and
				# 		(t['note']['expr'] == self.whois_name),
				# 	hsdev.client.types(files = [self.current_file_name]) or [])))

				# Try whois
				self.decl = head_of(hsdev.client.whois(self.whois_name, self.current_file_name))

				if not self.decl:
					self.decl = head_of(hsdev.client.lookup(self.full_name, self.current_file_name))

				if typed_expr or self.decl:
					popup_parts = [styles.gen_style(self.view.settings().get('color_scheme'))]
					if typed_expr:
						popup_parts.append(u'<p><span class="function">{0}</span>{1}</p>'.format(
							typed_expr['note']['expr'],
							symbols.format_type(u' :: {0}'.format(typed_expr['note']['type']))))
					if self.decl:
						popup_parts.append(self.decl.popup())
					popup_text = u''.join(popup_parts)
					if get_setting_async('unicode_symbol_info'):
						popup_text = popup_text.replace(html.escape('=>'), '\u21d2').replace(html.escape('->'), '\u2192').replace('::', '\u2237')
					self.view.show_popup(popup_text, sublime.HIDE_ON_MOUSE_MOVE_AWAY, point, 600, 600, self.on_navigate, self.on_hide)

		elif hover_zone == sublime.HOVER_GUTTER:
			self.view = view
			self.current_file_name = self.view.file_name()
			(line, column) = self.view.rowcol(point)
			errs = parseoutput.ERRORS[self.current_file_name][line]
			if errs:
				popup_parts = [styles.gen_style(self.view.settings().get('color_scheme'))]
				for err in errs:
					lines = []
					for line in err.message.splitlines():
						m = re.match(r'^\s+', line)
						if m:  # Replace leading spaces with non-breaking ones
							lines.append(m.end() * '&nbsp;' + html.escape(line[m.end():], quote = False))
						else:
							lines.append(html.escape(line, quote = False))

					msg = '<br>'.join(lines)
					# Decorate first word with style
					decors = {
						'Error': 'error',
						'Warning': 'warning',
						'Hint': 'hint'
					}
					for dec, dec_style in decors.items():
						msg = msg.replace(dec, u'<span class="{0}">{1}</span>'.format(dec_style, dec))
					popup_parts.append(u'<p>{0}</p>'.format(msg))
				popup_text = u''.join(popup_parts)
				self.view.show_popup(popup_text, sublime.HIDE_ON_MOUSE_MOVE_AWAY, point, 600, 600, None, None)

	def on_navigate(self, url):
		if self.view.is_popup_visible():
			self.view.hide_popup()
			if url[0:4] == 'http':
				webbrowser.open(url)
			else:
				self.view.window().open_file(url, sublime.ENCODED_POSITION | sublime.TRANSIENT)

	def on_hide(self):
		pass
