# -*- coding: UTF-8 -*-

import os
import re
import sublime
from threading import Thread

if int(sublime.version()) < 3000:
    import sublime_haskell_common as Common
    import internals.settings as Settings
    import internals.logging as Logging
    import ghci_backend as GHCIMod
    import hsdev as hsdev
    import parseoutput as ParseOutput
    import symbols
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.settings as Settings
    import SublimeHaskell.internals.logging as Logging
    import SublimeHaskell.ghci_backend as GHCIMod
    import SublimeHaskell.hsdev as hsdev
    import SublimeHaskell.parseoutput as ParseOutput
    import SublimeHaskell.symbols as symbols


def lint_as_hints(msgs):
    for msg in msgs:
        if msg[0] == 'lint':
            msg[1].level = 'hint'


def hsdev_check():
    return (hsdev.client.check, lambda file: [file], lambda ms: ms, {'ghc': Settings.get_setting_async('ghc_opts')})


def hsdev_lint():
    return (hsdev.client.lint, lambda file: [file], lambda ms: ms, {})

# def hsdev_check_lint():
#     return (hsdev.client.ghcmod_check_lint,
#             lambda file: [file], lambda ms: ms, { 'ghc': Settings.get_setting_async('ghc_opts') })


def messages_as_hints(cmd):
    (msg_fn, arg, _, kwargs) = cmd
    return (msg_fn, arg, lambda ms: [dict(m, level='hint') for m in ms], kwargs)


class SublimeHaskellHsDevChain(Common.SublimeHaskellTextCommand):
    def run(self, edit):
        pass

    def run_chain(self, cmds, msg, fly_mode=False):
        self.messages = []
        self.msgs = []
        self.corrections = []
        self.fly_mode = fly_mode
        self.filename = self.view.file_name()
        if not self.filename:
            return
        self.contents = {}
        if self.view.is_dirty():
            self.contents[self.filename] = self.view.substr(sublime.Region(0, self.view.size()))
        if not self.fly_mode:
            ParseOutput.hide_output(self.view)
        if not cmds:
            return
        else:
            self.status_msg = Common.status_message_process(msg + ': ' + self.filename, priority=2)
            self.status_msg.start()
            if not hsdev.agent_connected():
                Logging.log('hsdev chain fails: hsdev not connected', Logging.LOG_ERROR)
                self.status_msg.fail()
                self.status_msg.stop()
            else:
                self.go_chain(cmds)

    def go_chain(self, cmds):
        try:
            if not cmds:
                self.status_msg.stop()
                hsdev.client.autofix_show(self.msgs, on_response=self.on_autofix)
            else:
                cmd, tail_cmds = cmds[0], cmds[1:]
                (fn, modify_args, modify_msgs, kwargs) = cmd

                def on_resp(msgs):
                    self.messages.extend(modify_msgs(msgs))
                    self.msgs.extend(msgs)
                    self.go_chain(tail_cmds)

                def on_err(err, ds):
                    self.status_msg.fail()
                    self.go_chain([])

                fn(modify_args(self.filename),
                   contents=self.contents,
                   wait=False,
                   on_response=on_resp,
                   on_error=on_err, **kwargs)
        except Exception as e:
            Logging.log('hsdev chain fails with: {0}'.format(e), Logging.LOG_ERROR)
            self.status_msg.fail()
            self.status_msg.stop()

    def on_autofix(self, corrections):
        output_messages = [ParseOutput.OutputMessage(
            m['source']['file'],
            hsdev.parse_region(m['region']).to_zero_based(),
            m['level'].capitalize() + ': ' + m['note']['message'].replace('\n', '\n  '),
            m['level']) for m in self.messages]

        self.corrections = corrections
        for corr in self.corrections:
            corr.message_region.to_zero_based()
        self.corrections_dict = dict(((os.path.normpath(c.file), c.message_region.start.line, c.message_region.start.column), c)
                                     for c in self.corrections)

        for omsg in output_messages:
            okey = (os.path.normpath(omsg.filename), omsg.region.start.line, omsg.region.start.column)
            if okey in self.corrections_dict:
                omsg.correction = self.corrections_dict[okey]

        ParseOutput.set_global_error_messages(output_messages)
        output_text = ParseOutput.format_output_messages(output_messages)
        if Settings.get_setting_async('show_error_window'):
            sublime.set_timeout(lambda: ParseOutput.write_output(self.view,
                                                                 output_text,
                                                                 Common.get_cabal_project_dir_of_file(self.filename) or \
                                                                     os.path.dirname(self.filename),
                                                                 show_panel=not self.fly_mode and len(output_messages)),
                                0)
        sublime.set_timeout(lambda: ParseOutput.mark_messages_in_views(output_messages), 0)

    def is_enabled(self):
        return Common.is_haskell_source(None)


def ghcmod_command(cmdname):
    def wrap(fn):
        def wrapper(self, *args, **kwargs):
            if Settings.get_setting_async('enable_hsdev'):
                Logging.log("Invoking '{0}' command via hsdev".format(cmdname), Logging.LOG_TRACE)
                return fn(self, *args, **kwargs)
            elif Settings.get_setting_async('enable_ghc_mod'):
                Logging.log("Invoking '{0}' command via ghc-mod".format(cmdname), Logging.LOG_TRACE)
                self.view.window().run_command('sublime_haskell_ghc_mod_{0}'.format(cmdname))
            else:
                Common.show_status_message('Check/Lint: both hsdev and ghc-mod are disabled', False)
        return wrapper
    return wrap


class SublimeHaskellCheck(SublimeHaskellHsDevChain):
    @ghcmod_command('check')
    def run(self, edit, fly=False):
        self.run_chain([hsdev_check()], 'Checking', fly_mode=fly)


class SublimeHaskellLint(SublimeHaskellHsDevChain):
    @ghcmod_command('lint')
    def run(self, edit, fly=False):
        self.run_chain([hsdev_lint()], 'Linting', fly_mode=fly)


class SublimeHaskellCheckAndLint(SublimeHaskellHsDevChain):
    @ghcmod_command('check_and_lint')
    def run(self, edit, fly=False):
        self.run_chain([hsdev_check(), messages_as_hints(hsdev_lint())], 'Checking and Linting', fly_mode=fly)


class SublimeHaskellGhcModCheck(Common.SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmod(['check'], 'Checking')

    def is_enabled(self):
        return Common.is_haskell_source(None)


class SublimeHaskellGhcModLint(Common.SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmod(['lint', '-h', '-u'], 'Linting', lint_as_hints)

    def is_enabled(self):
        return Common.is_haskell_source(None)


class SublimeHaskellGhcModCheckAndLint(Common.SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmods([['check'], ['lint', '-h', '-u']], 'Checking and Linting', lint_as_hints)

    def is_enabled(self):
        return Common.is_haskell_source(None)


def run_ghcmods(cmds, msg, alter_messages_cb=None):
    """
    Run several ghcmod commands, concats result messages with callback
    and show output.
    alter_messages_cb accepts dictionary (cmd => list of output messages)
    """
    window, view, file_shown_in_view = Common.get_haskell_command_window_view_file_project()
    if not file_shown_in_view:
        return

    file_dir, file_name = os.path.split(file_shown_in_view)

    ghc_mod_args = []
    for cmd in cmds:
        ghc_mod_args.append((cmd, cmd + [file_shown_in_view]))

    def show_current_file_first_and_alter(msgs):
        if alter_messages_cb:
            alter_messages_cb(msgs)

        def sort_key(a):
            return (
                a[1].filename != file_shown_in_view,
                a[1].filename,
                a[1].start.line,
                a[1].start.column
            )

        msgs.sort(key=sort_key)

    run_ghcmods_thread(view, file_shown_in_view,
                       'Ghc-Mod: ' + msg + ' ' + file_name,
                       ghc_mod_args,
                       show_current_file_first_and_alter)


def run_ghcmod(cmd, msg, alter_messages_cb=None):
    run_ghcmods([cmd], msg, alter_messages_cb)


def run_ghcmods_thread(view, filename, msg, cmds_with_args, alter_messages_cb):
    sublime.status_message(msg + '...')
    thread = Thread(
        target=wait_ghcmod_and_parse,
        args=(view, filename, msg, cmds_with_args, alter_messages_cb))
    thread.start()


def wait_ghcmod_and_parse(view, filename, msg, cmds_with_args, alter_messages_cb):
    sublime.set_timeout(lambda: ParseOutput.hide_output(view), 0)

    parsed_messages = []

    file_dir = os.path.dirname(filename)

    all_cmds_successful = True
    all_cmds_outputs = []

    for (cmd, args) in cmds_with_args:
        stdout = GHCIMod.call_ghcmod_and_wait(args, filename)

        # stdout contains NULL as line endings within one message
        # error_output_regex using indents to determine one message scope
        # Replace NULLs to indents
        out = stdout.replace('\0', '\n  ')

        success = len(out.strip()) == 0

        if not success:
            all_cmds_outputs.append(out)
            Logging.log(u"ghc-mod %s didn't exit with success on '%s'" % (u' '.join(cmd), filename), Logging.LOG_ERROR)

        all_cmds_successful &= success

        parsed = ParseOutput.parse_output_messages(view, file_dir, out)
        for p in parsed:
            parsed_messages.append((cmd, p))

    if alter_messages_cb:
        alter_messages_cb(parsed_messages)

    concated_messages = [m[1] for m in parsed_messages]

    # Set global error list
    ParseOutput.set_global_error_messages(concated_messages)

    sublime.set_timeout(lambda: ParseOutput.mark_messages_in_views(concated_messages), 0)

    output_text = (ParseOutput.format_output_messages(concated_messages) if parsed_messages
                   else '\n'.join(all_cmds_outputs))

    exit_code = 0 if all_cmds_successful else 1

    ParseOutput.show_output_result_text(view, msg, output_text, exit_code, file_dir)


def ghcmod_browse_module(module_name, cabal=None):
    """
    Returns symbols.Module with all declarations
    """
    contents = GHCIMod.call_ghcmod_and_wait(['browse', '-d', module_name], cabal=cabal).splitlines()

    if not contents:
        return None

    m = symbols.Module(module_name, cabal=cabal)

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
