# -*- coding: UTF-8 -*-

import sublime
import sublime_plugin
import threading
import time

if int(sublime.version()) < 3000:
    from sublime_haskell_common import get_cabal_project_dir_and_name_of_view, get_setting, get_setting_async, is_haskell_source, LockedObject, log, log_trace
else:
    from SublimeHaskell.sublime_haskell_common import get_cabal_project_dir_and_name_of_view, get_setting, get_setting_async, is_haskell_source, LockedObject, log, log_trace

class SublimeHaskellAutobuild(sublime_plugin.EventListener):
    def __init__(self):
        super(SublimeHaskellAutobuild, self).__init__()
        self.fly_agent = FlyCheckLint()
        self.fly_agent.start()

    def on_post_save(self, view):
        auto_build_enabled = get_setting('enable_auto_build')
        auto_check_enabled = get_setting('enable_auto_check')
        auto_lint_enabled = get_setting('enable_auto_lint')
        cabal_project_dir, cabal_project_name = get_cabal_project_dir_and_name_of_view(view)

        # don't flycheck
        self.fly_agent.nofly()

        # auto build enabled and file within a cabal project
        if auto_build_enabled and cabal_project_dir is not None:
            view.window().run_command('sublime_haskell_build_auto')
        elif auto_check_enabled and auto_lint_enabled:
            view.window().run_command('sublime_haskell_check_and_lint')
        elif auto_check_enabled:
            view.window().run_command('sublime_haskell_check')
        elif auto_lint_enabled:
            view.window().run_command('sublime_haskell_lint')

    def on_modified(self, view):
        lint_check_fly = get_setting('lint_check_fly')
        auto_check_enabled = get_setting('enable_auto_check')
        auto_lint_enabled = get_setting('enable_auto_lint')

        if lint_check_fly and is_haskell_source(view) and view.file_name():
            self.fly_agent.fly(view)


class FlyCheckLint(threading.Thread):
    def __init__(self):
        super(FlyCheckLint, self).__init__()
        self.daemon = True
        self.view = LockedObject([])
        self.event = threading.Event()

    def fly(self, view):
        with self.view as v:
            v[:] = [view]
        self.event.set()

    def nofly(self):
        with self.view as v:
            v[:] = []
        self.event.set()

    def run(self):
        while True:
            self.event.wait()
            self.event.clear()
            time.sleep(5)
            view_ = None
            with self.view as v:
                if v:
                    view_ = v[0]
                v[:] = []
            if view_ is None:
                continue
            auto_check_enabled = get_setting_async('enable_auto_check')
            auto_lint_enabled = get_setting_async('enable_auto_lint')
            sublime.set_timeout(lambda: view_.window().run_command('sublime_haskell_scan_contents'), 0)
            if auto_check_enabled and auto_lint_enabled:
                sublime.set_timeout(lambda: view_.window().run_command('sublime_haskell_check_and_lint', {'fly': True}), 0)
            elif auto_check_enabled:
                sublime.set_timeout(lambda: view_.window().run_command('sublime_haskell_check', {'fly': True}), 0)
            elif auto_lint_enabled:
                sublime.set_timeout(lambda: view_.window().run_command('sublime_haskell_lint', {'fly': True}), 0)
