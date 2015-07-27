# -*- coding: UTF-8 -*-

import sublime
import sublime_plugin
import threading

if int(sublime.version()) < 3000:
    from sublime_haskell_common import attach_sandbox, get_cabal_project_dir_and_name_of_view, get_setting, is_haskell_source, LockedObject
else:
    from SublimeHaskell.sublime_haskell_common import attach_sandbox, get_cabal_project_dir_and_name_of_view, get_setting, is_haskell_source, LockedObject

fly_agent = None

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

    def on_modified(self, view):
        global fly_agent
        if fly_agent is None:
            fly_agent = FlyCheckLint()
            fly_agent.start()

        lint_check_fly = get_setting('lint_check_fly')
        auto_check_enabled = get_setting('enable_auto_check')
        auto_lint_enabled = get_setting('enable_auto_lint')

        if lint_check_fly and is_haskell_source(view) and view.file_name():
            fly_agent.modified(view)


class FlyCheckLint(threading.Thread):
    def __init__(self):
        super(FlyCheckLint, self).__init__()
        self.daemon = True
        self.view = LockedObject(None)
        self.fly_event = threading.Event()
        self.free_event = threading.Event()
        self.free()

    def modified(self, view):
        with self.view as v:
            self.view.object = view
        self.fly_event.set()

    def free(self):
        self.free_event.set()

    def run(self):
        while True:
            self.fly_event.wait()
            self.fly_event.clear()
            self.free_event.clear()

            with self.view as v:
                if v:
                    auto_check_enabled = get_setting('enable_auto_check')
                    auto_lint_enabled = get_setting('enable_auto_lint')
                    if auto_check_enabled and auto_lint_enabled:
                        v.window().run_command('sublime_haskell_check_and_lint', {'fly': True})
                    elif auto_check_enabled:
                        v.window().run_command('sublime_haskell_check', {'fly': True})
                    elif auto_lint_enabled:
                        v.window().run_command('sublime_haskell_lint', {'fly': True})

            self.free_event.wait(60.0)
            self.free_event.clear()

def fly_check_done():
    global fly_agent
    if fly_agent is not None:
        fly_agent.free()

def current_cabal_build():
    """Current cabal build command"""
    args = []
    if get_setting('use_cabal_sandbox'):
        args += ['cabal-dev']
    else:
        args += ['cabal']

    args += ['build']

    return attach_sandbox(args)
