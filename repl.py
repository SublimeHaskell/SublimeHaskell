import sublime
import sublime_plugin

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import autocomplete
    import symbols
    import hsdev
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.autocomplete as autocomplete
    import SublimeHaskell.symbols as symbols
    import SublimeHaskell.hsdev as hsdev
    import SublimeREPL.sublimerepl as sublimerepl



def find_sublime_haskell_repl():
    repls = list(sublimerepl.manager.find_repl('sublime_haskell_repl'))
    return repls

def run_repl_command(repl, str):
    repl.write("{0}\n".format(str))
    repl.repl.write("{0}\n".format(str))

def show_scope(repl):
    run_repl_command(":show modules")
    run_repl_command(":show imports")

class SublimeHaskellAutocompleteRepl(sublime_plugin.EventListener):
    def __init__(self):
        pass

    def repl_commands_completions(self):
        cmds = ["abandon", "add", "back", "break", "browse", "cd", "cmd", "complete", "continue", "ctags", "def", "delete", "edit", "etags", "force", "forward", "help", "history", "info", "issafe", "kind", "list", "load", "main", "module", "print", "quit", "r", "reload", "run", "script", "set", "seti", "show", "showi", "sprint", "step", "steplocal", "stepmodule", "trace", "type", "undef", "unset"]
        return [(":{0}".format(cmd), ":{0}".format(cmd)) for cmd in cmds]

    def on_query_completions(self, view, prefix, locations):
        if not is_haskell_repl(view):
            return []

        completions = self.repl_commands_completions()

        if get_setting('inhibit_completions') and len(completions) != 0:
            return (completions, sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS)
        return completions

def repl_args(**kwargs):
    def_args = {
        "type": "sublime_haskell",
        "encoding": "utf8",
        "cmd": ["ghci"],
        "cwd": "$file_path",
        "external_id": "sublime_haskell_repl",
        "syntax": "Packages/SublimeHaskell/Syntaxes/HaskellRepl.tmLanguage" }
    ret_args = def_args.copy()
    ret_args.update(kwargs)
    return ret_args

class SublimeHaskellReplGhci(SublimeHaskellWindowCommand):
    def run(self):
        self.window.run_command("repl_open", repl_args(cmd = ["ghci"]))

    def is_enabled(self):
        return True

class SublimeHaskellReplGhciCurrentFile(SublimeHaskellWindowCommand):
    def run(self):
        view = self.window.active_view()
        if not view:
            show_status_message("No file active", False)
        else:
            self.window.run_command("repl_open", repl_args(cmd = ["ghci", "$file"]))

class SublimeHaskellReplCabal(SublimeHaskellWindowCommand):
    def run(self):
        view = self.window.active_view()
        if not view:
            show_status_message("No file active", False)
        else:
            project_dir = get_cabal_project_dir_of_view(view)
            if not project_dir:
                show_status_message("Not in project", False)
            self.window.run_command("repl_open", repl_args(cmd = ["cabal", "repl"], cwd = project_dir))

    def is_enabled(self):
        return is_enabled_haskell_command(None, True)
