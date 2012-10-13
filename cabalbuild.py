import fnmatch
import os
import re
import sublime
import sublime_plugin
import subprocess
from threading import Thread
import time

from sublime_haskell_common import get_cabal_project_dir_of_view, get_cabal_project_dir_and_name_of_view, call_and_wait, log, are_paths_equal, get_setting, get_setting_async, set_setting, save_settings, is_enabled_haskell_command, get_haskell_command_window_view_file_project, SublimeHaskellBaseCommand
from parseoutput import run_chain_build_thread
from autobuild import attach_sandbox

cabal_tool = {
    True: { 'command': 'cabal-dev', 'message': 'Cabal-Dev', 'extra': lambda cmd: attach_sandbox(cmd) },
    False: { 'command': 'cabal', 'message': 'Cabal', 'extra': lambda cmd: cmd }
}

cabal_command = {
    'clean': { 'args': ['clean'], 'message': 'Cleaning' },
    'configure': { 'args': ['configure'], 'message': 'Configure' },
    'build': { 'args': ['build'], 'message': 'Building' },
    'rebuild': { 'args': ['clean', 'configure', 'build'], 'message': 'Rebuilding' },
    'install': { 'args': ['install'], 'message': 'Installing' }
}

def run_build(command, use_cabal_dev = None):
    # Run cabal or cabal-dev
    if use_cabal_dev == None:
        use_cabal_dev = get_setting_async('use_cabal_dev')

    tool = cabal_tool[use_cabal_dev]
    config = cabal_command[command]

    # Title of tool: Cabal, Cabal-Dev
    tool_title = tool['message']
    # Title of action: Cleaning, Building, etc.
    action_title = config['message']
    # Extra arguments lambda
    extra_args = tool['extra']
    # Tool name: cabal, cabal-dev
    tool_name = tool['command']
    # Tool arguments (commands): build, clean, etc.
    tool_args = config['args']

    run_build_commands_with(
        lambda name: tool_title + ': ' + action_title + ' ' + name,
        [extra_args([tool_name, arg]) for arg in tool_args])

class SublimeHaskellSwitchCabalDev(sublime_plugin.WindowCommand):
    def run(self):
        use_cabal_dev = get_setting('use_cabal_dev')
        set_setting('use_cabal_dev', not use_cabal_dev)
        if use_cabal_dev:
            now_using = 'Cabal'
        else:
            now_using = 'Cabal-Dev'
        sublime.status_message('SublimeHaskell: Switched to ' + now_using)
        save_settings()

# Default build system (cabal or cabal-dev)

class SublimeHaskellClean(SublimeHaskellBaseCommand):
    def run(self):
        run_build('clean')

class SublimeHaskellConfigure(SublimeHaskellBaseCommand):
    def run(self):
        run_build('configure')

class SublimeHaskellBuild(SublimeHaskellBaseCommand):
    def run(self):
        run_build('build')

class SublimeHaskellRebuild(SublimeHaskellBaseCommand):
    def run(self):
        run_build('rebuild')

class SublimeHaskellInstall(SublimeHaskellBaseCommand):
    def run(self):
        run_build('install')

# Cabal build system

class SublimeHaskellCabalClean(SublimeHaskellBaseCommand):
    def run(self):
        run_build('clean', False)

class SublimeHaskellCabalConfigure(SublimeHaskellBaseCommand):
    def run(self):
        run_build('configure', False)

class SublimeHaskellCabalBuild(SublimeHaskellBaseCommand):
    def run(self):
        run_build('build', False)

class SublimeHaskellCabalRebuild(SublimeHaskellBaseCommand):
    def run(self):
        run_build('configure', False)

class SublimeHaskellCabalInstall(SublimeHaskellBaseCommand):
    def run(self):
        run_build('install', False)

# Cabal-Dev build system

class SublimeHaskellCabalDevClean(SublimeHaskellBaseCommand):
    def run(self):
        run_build('clean', True)

class SublimeHaskellCabalDevConfigure(SublimeHaskellBaseCommand):
    def run(self):
        run_build('configure', True)

class SublimeHaskellCabalDevBuild(SublimeHaskellBaseCommand):
    def run(self):
        run_build('build', True)

class SublimeHaskellCabalDevRebuild(SublimeHaskellBaseCommand):
    def run(self):
        run_build('configure', True)

class SublimeHaskellCabalDevInstall(SublimeHaskellBaseCommand):
    def run(self):
        run_build('install', True)

def run_build_commands_with(msg, cmds):
    """Run general build commands"""
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project()
    if not file_shown_in_view:
        return
    syntax_file_for_view = view.settings().get('syntax').lower()
    if 'haskell' not in syntax_file_for_view:
        return
    cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(view)
    if not cabal_project_dir:
        return

    run_chain_build_thread(view, cabal_project_dir, msg(cabal_project_name), cmds)

def run_build_command_with(msg, cmd):
    """Run one command"""
    run_build_commands_with(msg, [cmd])
