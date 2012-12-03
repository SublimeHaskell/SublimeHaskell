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
            for m in msgs:
                if m[0] == 'lint':
                    m[1].level = 'hint'
        run_ghcmods(['check', 'lint'], 'Checking and Linting', lint_as_hints)

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
        ghc_mod_args.append((cmd, [cmd, file_shown_in_view]))

    def show_current_file_first_and_alter(msgs):
        if alter_messages_cb:
            alter_messages_cb(msgs)
        def compare(l, r):
            # sort by file equality to file_name
            res = cmp(l[1].filename != file_shown_in_view, r[1].filename != file_shown_in_view)
            if res == 0:
                # then by file
                res = cmp(l[1].filename, r[1].filename)
                if res == 0:
                    # then by line
                    res = cmp(l[1].line, r[1].line)
                    if res == 0:
                        # then by column
                        res = cmp(l[1].column, r[1].column)
            return res

        msgs.sort(compare)

    run_ghcmods_thread(view, file_shown_in_view, 'Ghc-Mod: ' + msg + ' ' + file_name, ghc_mod_args, show_current_file_first_and_alter)

def run_ghcmod(cmd, msg, alter_messages_cb = None):
    run_ghcmods([cmd], msg, alter_messages_cb)

def run_ghcmods_thread(view, filename, msg, cmds_with_args, alter_messages_cb):
    sublime.status_message(msg + '...')
    thread = Thread(
        target=wait_ghcmod_and_parse,
        args=(view, filename, msg, cmds_with_args, alter_messages_cb))
    thread.start()

def wait_ghcmod_and_parse(view, filename, msg, cmds_with_args, alter_messages_cb):
    sublime.set_timeout(lambda: hide_output(view), 0)

    exit_success = True

    parsed_messages = []

    file_dir = os.path.dirname(filename)

    for (cmd, args) in cmds_with_args:
        stdout = call_ghcmod_and_wait(args, filename)

        # stdout contains NULL as line endings within one message
        # error_output_regex using indents to determine one message scope
        # Replace NULLs to indents
        out = stdout.replace('\0', '\n  ').decode('utf-8')

        exit_success = exit_success and len(out) == 0

        parsed = parse_output_messages(file_dir, out)
        for p in parsed:
            parsed_messages.append((cmd, p))

    exit_code = 0 if exit_success else 1

    if alter_messages_cb:
        alter_messages_cb(parsed_messages)

    concated_messages = map(lambda m: m[1], parsed_messages)
    output_text = format_output_messages(concated_messages)

    show_output_result_text(view, msg, output_text, exit_code, file_dir)
    sublime.set_timeout(lambda: mark_messages_in_views(concated_messages), 0)
