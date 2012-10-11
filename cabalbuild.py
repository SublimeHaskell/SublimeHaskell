import fnmatch
import os
import re
import sublime
import sublime_plugin
import subprocess
from threading import Thread
import time

from sublime_haskell_common import get_cabal_project_dir_of_view, call_and_wait, log, are_paths_equal, get_setting
from autobuild import attach_sandbox, wait_for_build_to_complete, run_build_thread, run_chain_build_thread

class SublimeHaskellCabalBuild(sublime_plugin.WindowCommand):
	def run(self):
		run_build_command_with(
			'Cabal: Building Haskell',
			['cabal', 'build'])

	def is_enabled(self):
		return is_enabled_build_command()

class SublimeHaskellCabalClean(sublime_plugin.WindowCommand):
	def run(self):
		run_build_command_with(
			'Cabal: Cleaning Haskell',
			['cabal', 'clean'])

	def is_enabled(self):
		return is_enabled_build_command()

class SublimeHaskellCabalConfigure(sublime_plugin.WindowCommand):
	def run(self):
		run_build_command_with(
			'Cabal: Configuring Haskell',
			['cabal', 'configure'])

	def is_enabled(self):
		return is_enabled_build_command()

class SublimeHaskellCabalRebuild(sublime_plugin.WindowCommand):
	def run(self):
		run_build_commands_with(
			'Cabal: Rebuilding Haskell',
			[['cabal', 'clean'], ['cabal', 'configure'], ['cabal', 'build']])

	def is_enabled(self):
		return is_enabled_build_command()

class SublimeHaskellCabalDevBuild(sublime_plugin.WindowCommand):
	def run(self):
		run_build_command_with(
			'Cabal-Dev: Building Haskell',
			attach_sandbox(['cabal', 'build']))

	def is_enabled(self):
		return is_enabled_build_command()

class SublimeHaskellCabalDevClean(sublime_plugin.WindowCommand):
	def run(self):
		run_build_command_with(
			'Cabal-Dev: Cleaning Haskell',
			attach_sandbox(['cabal', 'clean']))

	def is_enabled(self):
		return is_enabled_build_command()

class SublimeHaskellCabalDevConfigure(sublime_plugin.WindowCommand):
	def run(self):
		run_build_command_with(
			'Cabal-Dev: Configuring Haskell',
			attach_sandbox(['cabal', 'configure']))

	def is_enabled(self):
		return is_enabled_build_command()

class SublimeHaskellCabalDevRebuild(sublime_plugin.WindowCommand):
	def run(self):
		run_build_commands_with(
			'Cabal: Rebuilding Haskell',
			[attach_sandbox(s) for s in [['cabal-dev', 'clean'], ['cabal-dev', 'configure'], ['cabal-dev', 'build']]])

	def is_enabled(self):
		return is_enabled_build_command()

def is_enabled_haskell_command():
	window = sublime.active_window()
	if not window:
		return False
	view = window.active_view()
	if view is None:
		return False
	file_shown_in_view = view.file_name()
	if file_shown_in_view is None:
		return False
	syntax_file_for_view = view.settings().get('syntax').lower()
	if 'haskell' not in syntax_file_for_view:
		return False
	return True

def is_enabled_build_command():
	window = sublime.active_window()
	if not window:
		return False
	view = window.active_view()
	if view is None:
		return False
	file_shown_in_view = view.file_name()
	if file_shown_in_view is None:
		return False
	syntax_file_for_view = view.settings().get('syntax').lower()
	if 'haskell' not in syntax_file_for_view:
		return False
	cabal_project_dir = get_cabal_project_dir_of_view(view)
	if cabal_project_dir is None:
		return False
	return True

def run_build_commands_with(msg, cmds):
	"""Run general build commands"""
	window = sublime.active_window()
	if not window:
		return
	view = window.active_view()
	if view is None:
		return
	file_shown_in_view = view.file_name()
	if file_shown_in_view is None:
		return
	syntax_file_for_view = view.settings().get('syntax').lower()
	if 'haskell' not in syntax_file_for_view:
		return
	cabal_project_dir = get_cabal_project_dir_of_view(view)
	if cabal_project_dir is None:
		return

	run_chain_build_thread(view, cabal_project_dir, msg, cmds)

def run_build_command_with(msg, cmd):
	"""Run one command"""
	run_build_command_with(msg, [cmd])
