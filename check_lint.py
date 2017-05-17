# -*- coding: UTF-8 -*-

import os
import pprint
import re
import threading

import sublime

import SublimeHaskell.cmdwin_types as CommandWin
import SublimeHaskell.ghcimod.ghci_backend as GHCIMod
import SublimeHaskell.hsdev.result_parse as HsResultParse
import SublimeHaskell.internals.backend_mgr as BackendMgr
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.parseoutput as ParseOutput
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.symbols as symbols


def lint_as_hints(msgs):
    for msg in msgs:
        if msg[0] == 'lint':
            msg[1].level = 'hint'


def file_as_file_list(file):
    '''Turn the file name into a singleton list. Used in hsdev_check() and hsdev_lint()
    '''
    return [file]

def unfiltered_messages(msgs):
    '''Identity function for message transforms.
    '''
    return msgs

def hsdev_check():
    return (BackendMgr.active_backend().check, file_as_file_list, unfiltered_messages, {'ghc': Settings.PLUGIN.ghc_opts})


def hsdev_lint():
    return (BackendMgr.active_backend().lint, file_as_file_list, unfiltered_messages, {})

# def hsdev_check_lint():
#     return (hsdev.client.ghcmod_check_lint,
#             lambda file: [file], lambda ms: ms, { 'ghc': Settings.PLUGIN.ghc_opts })


def messages_as_hints(cmd):
    (msg_fn, arg, _, kwargs) = cmd
    return (msg_fn, arg, lambda ms: [dict(m, level='hint') for m in ms], kwargs)


class SublimeHaskellHsDevChain(CommandWin.SublimeHaskellTextCommand):
    def __init__(self, view):
        super().__init__(view)
        self.messages = []
        self.msgs = []
        self.corrections = []
        self.corrections_dict = {}
        self.fly_mode = False
        self.filename = None
        self.contents = {}
        self.status_msg = None

    def run(self, edit):
        pass

    def run_chain(self, cmds, msg, fly_mode=False):
        self.messages = []
        self.msgs = []
        self.corrections = []
        self.fly_mode = fly_mode
        self.filename = self.view.file_name()
        if self.filename:
            self.contents = {}
            if self.view.is_dirty():
                self.contents[self.filename] = self.view.substr(sublime.Region(0, self.view.size()))
            if not self.fly_mode:
                ParseOutput.hide_output(self.view)
                if cmds:
                    self.status_msg = Common.status_message_process(msg + ': ' + self.filename, priority=2)
                    self.status_msg.start()
                    self.go_chain(cmds)
                else:
                    sublime.error_message('Empty command chain (check_lint.run_chain)')

    def go_chain(self, cmds):
        if not cmds:
            self.status_msg.stop()
            BackendMgr.active_backend().autofix_show(self.msgs, on_response=self.on_autofix)
        else:
            cmd, tail_cmds = cmds[0], cmds[1:]
            agent_func, modify_args, modify_msgs, kwargs = cmd

            def go_chain_resp(msgs):
                self.messages.extend(modify_msgs(msgs))
                self.msgs.extend(msgs)
                self.go_chain(tail_cmds)

            def go_chain_err(_err, _details):
                self.status_msg.fail()
                self.go_chain([])

            agent_func(modify_args(self.filename), contents=self.contents, wait_complete=False,
                       on_response=go_chain_resp, on_error=go_chain_err, **kwargs)

    def on_autofix(self, corrections):
        output_messages = [ParseOutput.OutputMessage(
            m['source']['file'],
            HsResultParse.parse_region(m['region']).to_zero_based(),
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
        if Settings.PLUGIN.show_error_window:
            cabal_proj_dir = Common.get_cabal_project_dir_of_file(self.filename) or os.path.dirname(self.filename)
            panel_display = not self.fly_mode and len(output_messages) > 0
            sublime.set_timeout(lambda: ParseOutput.write_output(self.view, output_text, cabal_proj_dir, panel_display), 0)
        sublime.set_timeout(lambda: ParseOutput.mark_messages_in_views(output_messages), 0)

    def is_enabled(self):
        return Common.is_haskell_source(None) and super().is_enabled()


def ghcmod_command(cmdname):
    def wrap(outer_fn):
        def wrapper(self, *args, **kwargs):
            Logging.log("Invoking '{0}' command via backend".format(cmdname), Logging.LOG_TRACE)
            return outer_fn(self, *args, **kwargs)
            # FIXME: Need to migrate into ghc-mod backend:
            # elif Settings.PLUGIN.enable_ghc_mod:
            #     Logging.log("Invoking '{0}' command via ghc-mod".format(cmdname), Logging.LOG_TRACE)
            #     self.view.window().run_command('sublime_haskell_ghc_mod_{0}'.format(cmdname))
        return wrapper
    return wrap


class SublimeHaskellCheck(SublimeHaskellHsDevChain):
    def __init__(self, view):
        super().__init__(view)

    @ghcmod_command('check')
    def run(self, edit, **kwargs):
        self.run_chain([hsdev_check()], 'Checking', fly_mode=(kwargs.get('fly') or False))


class SublimeHaskellLint(SublimeHaskellHsDevChain):
    def __init__(self, view):
        super().__init__(view)

    @ghcmod_command('lint')
    def run(self, edit, **kwargs):
        self.run_chain([hsdev_lint()], 'Linting', fly_mode=(kwargs.get('fly') or False))


class SublimeHaskellCheckAndLint(SublimeHaskellHsDevChain):
    def __init__(self, view):
        super().__init__(view)

    @ghcmod_command('check_and_lint')
    def run(self, edit, **kwargs):
        self.run_chain([hsdev_check(), messages_as_hints(hsdev_lint())],
                       'Checking and Linting',
                       fly_mode=(kwargs.get('fly') or False))


class SublimeHaskellGhcModCheck(CommandWin.SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmod(['check'], 'Checking')

    def is_enabled(self):
        return Common.is_haskell_source(None) and super().is_enabled()


class SublimeHaskellGhcModLint(CommandWin.SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmod(['lint', '-h', '-u'], 'Linting', lint_as_hints)

    def is_enabled(self):
        return Common.is_haskell_source(None) and super().is_enabled()


class SublimeHaskellGhcModCheckAndLint(CommandWin.SublimeHaskellWindowCommand):
    def run(self):
        run_ghcmods([['check'], ['lint', '-h', '-u']], 'Checking and Linting', lint_as_hints)

    def is_enabled(self):
        return Common.is_haskell_source(None) and super().is_enabled()


def run_ghcmods(cmds, msg, alter_messages_cb=None):
    """
    Run several ghcmod commands, concats result messages with callback
    and show output.
    alter_messages_cb accepts dictionary (cmd => list of output messages)
    """
    _window, view, file_shown_in_view = Common.get_haskell_command_window_view_file_project()
    if not file_shown_in_view:
        return

    file_name = os.path.split(file_shown_in_view)[1]

    ghc_mod_args = []
    for cmd in cmds:
        ghc_mod_args.append((cmd, cmd + [file_shown_in_view]))

    def show_current_file_first_and_alter(msgs):
        if alter_messages_cb:
            alter_messages_cb(msgs)

        def sort_key(msg):
            return (msg[1].filename != file_shown_in_view,
                    msg[1].filename,
                    msg[1].start.line,
                    msg[1].start.column)

        msgs.sort(key=sort_key)

    run_ghcmods_thread(view, file_shown_in_view,
                       'Ghc-Mod: ' + msg + ' ' + file_name,
                       ghc_mod_args,
                       show_current_file_first_and_alter)


def run_ghcmod(cmd, msg, alter_messages_cb=None):
    run_ghcmods([cmd], msg, alter_messages_cb)


def run_ghcmods_thread(view, filename, msg, cmds_with_args, alter_messages_cb):
    sublime.status_message(msg + '...')
    thread = threading.Thread(target=wait_ghcmod_and_parse, args=(view, filename, msg, cmds_with_args, alter_messages_cb))
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

        for parsed in ParseOutput.parse_output_messages(view, file_dir, out):
            parsed_messages.append((cmd, parsed))

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

    mod_decls = symbols.Module(module_name)

    function_regex = r'(?P<name>\w+)\s+::\s+(?P<type>.*)'
    type_regex = r'(?P<what>(class|type|data|newtype))\s+(?P<name>\w+)(\s+(?P<args>\w+(\s+\w+)*))?'

    def to_decl(line):
        matched = re.search(function_regex, line)
        if matched:
            return symbols.Function(matched.group('name'), matched.group('type'))
        else:
            matched = re.search(type_regex, line)
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

    for decl in map(to_decl, contents):
        mod_decls.add_declaration(decl)

    return mod_decls
