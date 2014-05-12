import sublime
import sublime_plugin
import sys

if int(sublime.version()) < 3000:
	from sublime_haskell_common import *
	import hsdev
	from parseoutput import write_panel
	from autocomplete import hsdev_client
else:
	from SublimeHaskell.sublime_haskell_common import *
	import SublimeHaskell.hsdev as hsdev
	from SublimeHaskell.parseoutput import write_panel
	from SublimeHaskell.autocomplete import hsdev_client

class SublimeHaskellCabalList(SublimeHaskellWindowCommand):
	def run(self):
		self.window.show_input_panel("Cabal list", "", self.on_done, self.on_change, self.on_cancel)

	def on_done(self, input):
		self.packages = hsdev_client().cabal_list(input)
		if not self.packages:
			show_status_message("Package {0} not found".format(input))
			return

		self.window.show_quick_panel([([p.name] + ([p.synopsis] if p.synopsis else [])) for p in self.packages], self.on_select)

	def on_change(self, input):
		pass

	def on_cancel(self):
		pass

	def on_select(self, idx):
		if idx == -1:
			return
		package = self.packages[idx]

		write_panel(self.window, package.detailed(), 'sublime_haskell_cabal_package')
