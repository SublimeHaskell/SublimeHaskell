import os
import re
import sublime
import sublime_plugin
from threading import Thread
import time

from sublime_haskell_common import log, is_enabled_haskell_command, get_haskell_command_window_view_file_project, try_attach_sandbox, call_ghcmod_and_wait
from parseoutput import parse_output_messages_and_show, hide_output, OutputMessage

class SublimeHaskellGhcModCheck(sublime_plugin.WindowCommand):
    def run(self):
        run_ghcmod('check', 'Checking')

    def is_enabled(self):
        return is_enabled_haskell_command(False)

class SublimeHaskellGhcModLint(sublime_plugin.WindowCommand):
    def run(self):
        run_ghcmod('lint', 'Linting')

    def is_enabled(self):
        return is_enabled_haskell_command(False)

class SublimeHaskellGhcModCheckAndLint(sublime_plugin.WindowCommand):
    def run(self):
        pass

    def is_enabled(self):
        return is_enabled_haskell_command(False)

def run_ghcmod(cmd, msg):
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project()
    if not file_shown_in_view:
        return

    file_dir, file_name = os.path.split(file_shown_in_view)

    ghc_mod_args = [cmd, file_name]

    run_ghcmod_thread(view, file_dir, 'Ghc-Mod: ' + msg + ' ' + file_name, ghc_mod_args)

def run_ghcmod_thread(view, file_dir, msg, arg_list):
    sublime.status_message(msg + '...')
    thread = Thread(
        target=wait_ghcmod_and_parse,
        args=(view, file_dir, msg, arg_list))
    thread.start()

def wait_ghcmod_and_parse(view, file_dir, msg, arg_list):
    sublime.set_timeout(lambda: hide_output(view), 0)

    stdout = call_ghcmod_and_wait(arg_list, file_dir)
 
    # stdout contains NULL as line endings within one message
    # error_output_regex using indents to determine one message scope
    # Replace NULLs to indents
    out = stdout.replace('\0', '\n  ')

    exit_code = 0 if len(out) == 0 else 1

    parse_output_messages_and_show(
        view,
        msg,
        file_dir,
        exit_code,
        '',
        out)
