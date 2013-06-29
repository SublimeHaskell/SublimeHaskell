import os
import re
import sublime
import sublime_plugin
from threading import Thread

if int(sublime.version()) < 3000:
    from sublime_haskell_common import log, is_enabled_haskell_command, get_haskell_command_window_view_file_project, call_ghcmod_and_wait, get_setting_async
    from parseoutput import parse_output_messages, show_output_result_text, format_output_messages, mark_messages_in_views, hide_output, set_global_error_messages
    from ghci import parse_info
    import symbols
else:
    from SublimeHaskell.sublime_haskell_common import log, is_enabled_haskell_command, get_haskell_command_window_view_file_project, call_ghcmod_and_wait, get_setting_async
    from SublimeHaskell.parseoutput import parse_output_messages, show_output_result_text, format_output_messages, mark_messages_in_views, hide_output, set_global_error_messages
    from SublimeHaskell.ghci import parse_info
    import SublimeHaskell.symbols as symbols


def lint_as_hints(msgs):
    for m in msgs:
        if m[0] == 'lint':
            m[1].level = 'hint'


class SublimeHaskellGhcModCheck(sublime_plugin.WindowCommand):
    def run(self):
        run_ghcmod(['check'], 'Checking')

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)


class SublimeHaskellGhcModLint(sublime_plugin.WindowCommand):
    def run(self):
        run_ghcmod(['lint', '-h', '-u'], 'Linting', lint_as_hints)

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)


class SublimeHaskellGhcModCheckAndLint(sublime_plugin.WindowCommand):
    def run(self):
        run_ghcmods([['check'], ['lint', '-h', '-u']], 'Checking and Linting', lint_as_hints)

    def is_enabled(self):
        return is_enabled_haskell_command(None, False)


def run_ghcmods(cmds, msg, alter_messages_cb=None):
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
        ghc_mod_args.append((cmd, cmd + [file_shown_in_view]))

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

        def sort_key(a):
            return (
                a[1].filename != file_shown_in_view,
                a[1].filename,
                a[1].line,
                a[1].column
            )

        msgs.sort(key=sort_key)

    run_ghcmods_thread(view, file_shown_in_view, 'Ghc-Mod: ' + msg + ' ' + file_name, ghc_mod_args, show_current_file_first_and_alter)


def run_ghcmod(cmd, msg, alter_messages_cb=None):
    run_ghcmods([cmd], msg, alter_messages_cb)


def run_ghcmods_thread(view, filename, msg, cmds_with_args, alter_messages_cb):
    sublime.status_message(msg + '...')
    thread = Thread(
        target=wait_ghcmod_and_parse,
        args=(view, filename, msg, cmds_with_args, alter_messages_cb))
    thread.start()


def wait_ghcmod_and_parse(view, filename, msg, cmds_with_args, alter_messages_cb):
    sublime.set_timeout(lambda: hide_output(view), 0)

    parsed_messages = []

    file_dir = os.path.dirname(filename)

    all_cmds_successful = True
    all_cmds_outputs = []

    for (cmd, args) in cmds_with_args:
        stdout = call_ghcmod_and_wait(args, filename)

        # stdout contains NULL as line endings within one message
        # error_output_regex using indents to determine one message scope
        # Replace NULLs to indents
        out = stdout.replace('\0', '\n  ')

        success = len(out) == 0

        if not success:
            all_cmds_outputs.append(out)
            log(u"ghc-mod %s didn't exit with success on '%s'" % (u' '.join(cmd), filename))

        all_cmds_successful &= success

        parsed = parse_output_messages(file_dir, out)
        for p in parsed:
            parsed_messages.append((cmd, p))

    if alter_messages_cb:
        alter_messages_cb(parsed_messages)

    concated_messages = [m[1] for m in parsed_messages]

    # Set global error list
    set_global_error_messages(concated_messages)

    sublime.set_timeout(lambda: mark_messages_in_views(concated_messages), 0)

    output_text = (format_output_messages(concated_messages) if parsed_messages
                   else '\n'.join(all_cmds_outputs))

    exit_code = 0 if all_cmds_successful else 1

    show_output_result_text(view, msg, output_text, exit_code, file_dir)

def ghcmod_browse_module(module_name, cabal = None):
    """
    Returns symbols.Module with all declarations
    """
    contents = call_ghcmod_and_wait(['browse', '-d', module_name], cabal = cabal).splitlines()

    if not contents:
        return None

    m = symbols.Module(module_name, cabal = cabal)

    functionRegex = r'(?P<name>\w+)\s+::\s+(?P<type>.*)'
    typeRegex = r'(?P<what>(class|type|data|newtype))\s+(?P<name>\w+)(\s+(?P<args>\w+(\s+\w+)*))?'

    def toDecl(line):
        matched = re.search(functionRegex, line)
        if matched:
            return symbols.Function(matched.group('name'), matched.group('type'))
        else:
            matched = re.search(typeRegex, line)
            if matched:
                decl_type = matched.group('what')
                decl_name = matched.group('name')
                decl_args = matched.group('args')
                decl_args = decl_args.split() if decl_args else []

                if decl_type == 'class':
                    return symbols.Class(decl_name, None, decl_args)
                elif decl_type == 'data':
                    return symbols.Data(decl_name, None, decl_args)
                elif decl_type == 'type':
                    return symbols.Type(decl_name, None, decl_args)
                elif decl_type == 'newtype':
                    return symbols.Newtype(decl_name, None, decl_args)
            else:
                return symbols.Declaration(line)

    decls = map(toDecl, contents)
    for decl in decls:
        m.add_declaration(decl)

    return m

def ghcmod_info(filename, module_name, symbol_name, cabal = None):
    """
    Uses ghc-mod info filename module_name symbol_name to get symbol info
    """
    contents = call_ghcmod_and_wait(['info', filename, module_name, symbol_name], filename = filename, cabal = cabal)
    # TODO: Returned symbol doesn't contain location
    # But in fact we use ghcmod_info only to retrieve type of symbol
    return parse_info(symbol_name, contents)

def ghcmod_type(filename, module_name, line, column, cabal = None):
    """
    Uses ghc-mod type to infer type
    """
    return call_ghcmod_and_wait(['type', filename, module_name, str(line), str(column)], filename = filename, cabal = cabal)

def ghcmod_enabled():
    return get_setting_async('enable_ghc_mod') == True
