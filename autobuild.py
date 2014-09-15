import sublime
import sublime_plugin

if int(sublime.version()) < 3000:
    from sublime_haskell_common import attach_sandbox, get_cabal_project_dir_and_name_of_view, get_setting
else:
    from SublimeHaskell.sublime_haskell_common import attach_sandbox, get_cabal_project_dir_and_name_of_view, get_setting


class SublimeHaskellAutobuild(sublime_plugin.EventListener):
    def on_post_save(self, view):
        auto_build_enabled = get_setting('enable_auto_build')
        auto_check_enabled = get_setting('enable_auto_check')
        auto_lint_enabled = get_setting('enable_auto_lint')
        cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(view)

        # auto build enabled and file within a cabal project
        if auto_build_enabled and cabal_project_dir is not None:
            view.window().run_command('sublime_haskell_build_auto')
        # try to ghc-mod check
        elif get_setting('enable_ghc_mod'):
            if auto_check_enabled and auto_lint_enabled:
                view.window().run_command('sublime_haskell_check_and_lint')
            elif auto_check_enabled:
                view.window().run_command('sublime_haskell_check')
            elif auto_lint_enabled:
                view.window().run_command('sublime_haskell_lint')


def current_cabal_build():
    """Current cabal build command"""
    args = []
    if get_setting('use_cabal_sandbox'):
        args += ['cabal-dev']
    else:
        args += ['cabal']

    args += ['build']

    return attach_sandbox(args)
