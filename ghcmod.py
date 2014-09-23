import os
import re
import sublime
import sublime_plugin
import threading
from threading import Thread

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import autocomplete
    from parseoutput import OutputMessage, parse_output_messages, show_output_result_text, format_output_messages, mark_messages_in_views, hide_output, set_global_error_messages, write_output
    from ghci import parse_info
    import symbols
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.autocomplete as autocomplete
    from SublimeHaskell.parseoutput import OutputMessage, parse_output_messages, show_output_result_text, format_output_messages, mark_messages_in_views, hide_output, set_global_error_messages, write_output
    from SublimeHaskell.ghci import parse_info
    import SublimeHaskell.symbols as symbols



def lint_as_hints(msgs):
    for m in msgs:
        if m[0] == 'lint':
            m[1].level = 'hint'



def hsdev_check():
    return (autocomplete.hsdev_client.ghcmod_check, lambda file: [file], lambda ms: ms)
def hsdev_lint():
    return (autocomplete.hsdev_client.ghcmod_lint, lambda file: file, lambda ms: ms)

def messages_as_hints(cmd):
    (fn, arg, msg) = cmd
    return (fn, arg, lambda ms: [dict(m, level = 'hint') for m in ms])

class SublimeHaskellGhcModChain(SublimeHaskellTextCommand):
    def run(self, edit):
        pass

    def run_chain(self, cmds, msg):
        self.messages = []
        self.filename = self.view.file_name()
        hide_output(self.view)
        if not cmds:
            return
        else:
            self.status_msg = status_message_process(msg + ': ' + self.filename)
            self.status_msg.start()
            self.go_chain(cmds)

    def go_chain(self, cmds):
        try:
            if not cmds:
                self.status_msg.stop()
                output_messages = [OutputMessage(
                    m['location']['module']['file'],
                    m['location']['pos']['line'],
                    m['location']['pos']['column'],
                    m['level'].capitalize() + ': ' + m['message'].replace('\n', '\n  '),
                    m['level']) for m in self.messages]

                set_global_error_messages(output_messages)
                output_text = format_output_messages(output_messages)
                if output_text:
                    if get_setting_async('show_output_window'):
                        sublime.set_timeout(lambda: write_output(
                            self.view,
                            output_text,
                            get_cabal_project_dir_of_file(self.filename) or os.path.dirname(self.filename)))
                    sublime.set_timeout(lambda: mark_messages_in_views(output_messages), 0)
            else:
                cmd, tail_cmds = cmds[0], cmds[1:]
                (fun, modify_arg, modify_messages) = cmd

                def on_resp(msgs):
                    self.messages.extend(modify_messages(msgs))
                    self.go_chain(tail_cmds)

                def on_err(err):
                    self.status_msg.fail()
                    self.go_chain([])

                fun(modify_arg(self.filename), wait = False, on_response = on_resp, on_error = on_err)
        except Exception as e:
            log('hsdev ghc-mod chain fails with: {0}'.format(e), log_error)
            self.status_msg.stop()

    def is_enabled(self):
        return is_haskell_source(None)



def ghcmod_command(cmdname):
    def wrap(fn):
        def wrapper(self, *args, **kwargs):
            if autocomplete.hsdev_agent_connected():
                log('Invoking ghc-mod command \'{0}\' via hsdev'.format(cmdname), log_trace)
                return fn(self, *args, **kwargs)
            else:
                log('Invoking ghc-mod command \'{0}\' via ghc-mod'.format(cmdname), log_trace)
                self.view.window().run_command('sublime_haskell_ghc_mod_{0}'.format(cmdname))
        return wrapper
    return wrap

class SublimeHaskellCheck(SublimeHaskellGhcModChain):
    @ghcmod_command('check')
    def run(self, edit):
        self.run_chain([hsdev_check()], 'Checking')

class SublimeHaskellLint(SublimeHaskellGhcModChain):
    @ghcmod_command('lint')
    def run(self, edit):
        self.run_chain([hsdev_lint()], 'Linting')

class SublimeHaskellCheckAndLint(SublimeHaskellGhcModChain):
    @ghcmod_command('check_and_lint')
    def run(self, edit):
        self.run_chain([hsdev_check(), messages_as_hints(hsdev_lint())], 'Checking and Linting')



class SublimeHaskellGhcModCheck(SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmod(['check'], 'Checking')

    def is_enabled(self):
        return is_haskell_source(None)


class SublimeHaskellGhcModLint(SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmod(['lint', '-h', '-u'], 'Linting', lint_as_hints)

    def is_enabled(self):
        return is_haskell_source(None)


class SublimeHaskellGhcModCheckAndLint(SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmods([['check'], ['lint', '-h', '-u']], 'Checking and Linting', lint_as_hints)

    def is_enabled(self):
        return is_haskell_source(None)


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

        success = len(out.strip()) == 0

        if not success:
            all_cmds_outputs.append(out)
            log(u"ghc-mod %s didn't exit with success on '%s'" % (u' '.join(cmd), filename), log_error)

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
