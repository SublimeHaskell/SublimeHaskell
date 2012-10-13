import os
import re
import sublime
import sublime_plugin
from threading import Thread
import time

from sublime_haskell_common import log, is_enabled_haskell_command, get_haskell_command_window_view_file_project, try_attach_sandbox, call_ghcmod_and_wait
from parseoutput import parse_output_messages, show_output_result_text, format_output_messages, mark_messages_in_views, parse_output_messages_and_show, hide_output, OutputMessage

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
        def lint_as_hints(msgs):
            for m in msgs['lint']:
                m.level = 'hint'
        run_ghcmods(['check', 'lint'], 'Checking and Lintind', lint_as_hints)

    def is_enabled(self):
        return is_enabled_haskell_command(False)

def run_ghcmods(cmds, msg, alter_messages_cb = None):
    """
    Run several ghcmod commands, concats result messages with callback
    and show output.
    alter_messages_cb accepts dictionary (cmd => list of output messages)
    """
    window, view, file_shown_in_view = get_haskell_command_window_view_file_project()
    if not file_shown_in_view:
        return

    file_dir, file_name = os.path.split(file_shown_in_view)

    ghc_mod_args = []
    for cmd in cmds:
        ghc_mod_args.append((cmd, [cmd, file_name]))

    run_ghcmods_thread(view, file_dir, 'Ghc-Mod: ' + msg + ' ' + file_name, ghc_mod_args, alter_messages_cb)

def run_ghcmod(cmd, msg, alter_message_cb = None):
    def alter_messages_cb(msgs):
        return map(alter_message_cb, msgs[cmd])
    run_ghcmods([cmd], msg, alter_messages_cb if alter_message_cb else None)

def run_ghcmods_thread(view, file_dir, msg, cmds_with_args, alter_messages_cb):
    sublime.status_message(msg + '...')
    thread = Thread(
        target=wait_ghcmod_and_parse,
        args=(view, file_dir, msg, cmds_with_args, alter_messages_cb))
    thread.start()

def wait_ghcmod_and_parse(view, file_dir, msg, cmds_with_args, alter_messages_cb):
    sublime.set_timeout(lambda: hide_output(view), 0)

    exit_success = True

    parsed_messages = {}

    for (cmd, args) in cmds_with_args:
        stdout = call_ghcmod_and_wait(args, file_dir)

        # stdout contains NULL as line endings within one message
        # error_output_regex using indents to determine one message scope
        # Replace NULLs to indents
        out = stdout.replace('\0', '\n  ').decode('utf-8')

        exit_success = exit_success and len(out) == 0

        parsed_messages[cmd] = parse_output_messages(file_dir, out)

    exit_code = 0 if exit_success else 1

    def concat_messages(ms):
        res = []
        for (c, a) in cmds_with_args:
            res.extend(ms[c])
        return res

    if alter_messages_cb:
        alter_messages_cb(parsed_messages)

    concated_messages = concat_messages(parsed_messages)
    output_text = format_output_messages(concated_messages)

    show_output_result_text(view, msg, output_text, exit_code, file_dir)
    sublime.set_timeout(lambda: mark_messages_in_views(concated_messages), 0)
