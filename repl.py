# -*- coding: UTF-8 -*-

import os
import re

import sublime
import sublime_plugin

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.autocomplete as autocomplete
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.hsdev.agent as hsdev

HAS_SUBLIME_REPL = True

try:
    import SublimeREPL.sublimerepl as sublimerepl
except ImportError:
    Logging.log('SublimeREPL is not installed, ghci/repl commands disabled', Logging.LOG_INFO)
    HAS_SUBLIME_REPL = False


COMMAND_RE = re.compile(r'^.*:[a-z]*$')
IMPORT_RE = re.compile(r'^.*\bimport\s+(qualified\s+)?(?P<module>[\w\d\.]*)$')


def find_sublime_haskell_repl():
    known_repls = list(sublimerepl.manager.find_repl('sublime_haskell_repl')) if HAS_SUBLIME_REPL else []
    return known_repls


def run_repl_command(repl, repl_cmd):
    repl.write("{0}\n".format(repl_cmd))
    repl.repl.write("{0}\n".format(repl_cmd))


def show_scope(repl):
    run_repl_command(repl, ":show modules")
    run_repl_command(repl, ":show imports")


class Repl(object):
    def __init__(self, view, path=None, project_name=None):
        self.view = view
        self.path = path
        if not path:
            path = os.path.dirname(view.file_name())
        self.project_name = project_name

    def is_project(self):
        return self.project_name is not None


# external_id => view
class Repls(object):
    def __init__(self):
        self.repls = {}

    def set_repl_view(self, repl_req, view, **kwargs):
        self.repls[repl_req] = Repl(view, **kwargs)

    def get_repl_view(self, repl_req):
        return self.repls.get(repl_req)

KNOWN_REPLS = Repls()


class SublimeHaskellAutocompleteRepl(sublime_plugin.EventListener):
    def __init__(self):
        pass

    def repl_commands_completions(self):
        cmds = ["abandon", "add", "back", "break", "browse", "cd", "cmd", "complete", "continue", "ctags", "def", "delete",
                "edit", "etags", "force", "forward", "help", "history", "info", "issafe", "kind", "list", "load", "main",
                "module", "print", "quit", "r", "reload", "run", "script", "set", "seti", "show", "showi", "sprint", "step",
                "steplocal", "stepmodule", "trace", "type", "undef", "unset"]
        return [(":{0}".format(cmd), ":{0}".format(cmd)) for cmd in cmds]

    def on_query_completions(self, view, _prefix, locations):
        if not HAS_SUBLIME_REPL or not Common.is_haskell_repl(view):
            return []

        repl = sublimerepl.manager.repl_view(view)

        line_contents = Common.get_line_contents(view, locations[0])
        command = COMMAND_RE.match(line_contents)
        if command:
            return self.repl_commands_completions()

        imp = IMPORT_RE.match(line_contents)
        if imp:
            mod = imp.group('module')
            repl_id = KNOWN_REPLS.get_repl_view(repl.external_id)
            cwd = repl_id.path if repl_id else None

            return (autocomplete.AUTO_COMPLETER.get_module_completions_for(mod, current_dir=cwd),
                    sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)

        # ????
        completions = []

        if Settings.PLUGIN.inhibit_completions and len(completions) != 0:
            return (completions, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        return completions


def repl_args(**kwargs):
    def_args = {"type": "sublime_haskell",
                "encoding": "utf8",
                "cmd": ["ghci"],
                "cwd": "$file_path",
                "external_id": "sublime_haskell_repl",
                "syntax": "Packages/SublimeHaskell/Syntaxes/HaskellRepl.tmLanguage"}

    # Drop this options until https://github.com/wuub/SublimeREPL/pull/395 is merged
    kwargs.pop('loaded')
    kwargs.pop('caption')

    ret_args = def_args.copy()
    ret_args.update(kwargs)
    return ret_args


class SublimeHaskellReplGhci(Common.SublimeHaskellWindowCommand):
    def run(self):
        opts = Settings.PLUGIN.ghci_opts or []
        self.window.run_command("repl_open", repl_args(cmd=["ghci"] + opts, loaded=None, caption="ghci"))

    def is_enabled(self):
        return HAS_SUBLIME_REPL


class SublimeHaskellReplGhciCurrentFile(Common.SublimeHaskellWindowCommand):
    def run(self):
        view = self.window.active_view()
        if not view:
            Common.show_status_message("No file active", False)
        else:
            opts = Settings.PLUGIN.ghci_opts or []
            self.window.run_command("repl_open", repl_args(cmd=["ghci", "$file"] + opts,
                                                           loaded=view.file_name(),
                                                           caption="ghci: {0}".format(os.path.basename(view.file_name()))))
            KNOWN_REPLS.set_repl_view(sublimerepl.repl_external_id(view.file_name()), view)

    def is_enabled(self):
        return HAS_SUBLIME_REPL and Common.SublimeHaskellWindowCommand.is_enabled(self)


class SublimeHaskellReplCabal(Common.SublimeHaskellWindowCommand):
    def __init__(self, window):
        super().__init__(window)
        self.view = None
        self.project_name = None
        self.project_dir = None
        self.names = []
    def run(self):
        self.view = self.window.active_view()
        if not self.view:
            Common.show_status_message("No file active", False)
        else:
            project_dir, project_name = Common.get_cabal_project_dir_and_name_of_view(self.view)
            if not project_dir:
                Common.show_status_message("Not in project", False)
            proj_info = hsdev.client.project(project_name)
            self.project_name = project_name
            self.project_dir = project_dir
            self.names = ['lib:{0}'.format(project_name)]
            if proj_info:
                self.names.extend(['exe:{0}'.format(executable['name'])
                                   for executable in proj_info['description']['executables']])
                self.names.extend(['test:{0}'.format(test['name'])
                                   for test in proj_info['description']['tests']])
            if len(self.names) > 1:
                self.window.show_quick_panel(self.names, self.on_done)
            else:
                self.on_done(0)

    def on_done(self, idx):
        if idx >= 0:
            capt = "cabal repl: {0}/{1}".format(self.project_name, self.names[idx])
            self.window.run_command("repl_open", repl_args(cmd=["cabal", "repl", self.names[idx]],
                                                           cwd=self.project_dir,
                                                           loaded=self.project_dir,
                                                           caption=capt))
            KNOWN_REPLS.set_repl_view(sublimerepl.repl_external_id(self.project_dir),
                                      self.view,
                                      path=self.project_dir,
                                      project_name=self.project_name)

    def is_enabled(self):
        return HAS_SUBLIME_REPL and Common.is_enabled_haskell_command(None, True)


class SublimeHaskellReplLoad(Common.SublimeHaskellWindowCommand):
    def run(self):
        view = self.window.active_view()
        if not view:
            Common.show_status_message("No file active", False)
        else:
            project_dir = Common.get_cabal_project_dir_and_name_of_view(view)[0]
            if not project_dir:
                self.window.run_command("sublime_haskell_repl_ghci_current_file", {})
            else:
                self.window.run_command("sublime_haskell_repl_cabal", {})

    def is_enabled(self):
        return HAS_SUBLIME_REPL and Common.SublimeHaskellWindowCommand.is_enabled(self)
