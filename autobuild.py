# -*- coding: UTF-8 -*-

import threading
import time

import sublime
import sublime_plugin

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.settings as Settings


class SublimeHaskellAutobuild(sublime_plugin.EventListener):
    def __init__(self):
        super().__init__()
        self.fly_agent = FlyCheckLint()
        self.fly_agent.start()

    def on_post_save(self, view):
        cabal_project_dir, _ = Common.get_cabal_project_dir_and_name_of_view(view)

        # don't flycheck
        self.fly_agent.nofly()

        # auto build enabled and file within a cabal project
        if Settings.PLUGIN.enable_auto_build and cabal_project_dir is not None:
            view.window().run_command('sublime_haskell_build_auto')
        elif Settings.PLUGIN.enable_auto_check and Settings.PLUGIN.enable_auto_lint:
            view.window().run_command('sublime_haskell_check_and_lint')
            view.window().run_command('sublime_haskell_get_types')
        elif Settings.PLUGIN.enable_auto_check:
            view.window().run_command('sublime_haskell_check')
            view.window().run_command('sublime_haskell_get_types')
        elif Settings.PLUGIN.enable_auto_lint:
            view.window().run_command('sublime_haskell_lint')

    def on_modified(self, view):
        lint_check_fly = Settings.PLUGIN.lint_check_fly

        if lint_check_fly and Common.is_haskell_source(view) and view.file_name():
            self.fly_agent.fly(view)


class FlyCheckLint(threading.Thread):
    def __init__(self):
        super().__init__()
        self.daemon = True
        self.view = LockedObject.LockedObject({'view':None, 'mtime':None})
        self.event = threading.Event()

    def fly(self, view):
        with self.view as view:
            view['view'] = view
            view['mtime'] = time.time()
        self.event.set()

    def nofly(self):
        with self.view as view:
            view['view'] = None
            view['mtime'] = None
        self.event.set()

    def run(self):
        while True:
            view_ = None
            mtime_ = None
            delay = Settings.PLUGIN.lint_check_fly_idle

            with self.view as view:
                view_ = view['view']
                mtime_ = view['mtime']

            if not view_:  # Wait for signal
                self.event.wait()
                self.event.clear()
                time.sleep(delay)
                continue

            if time.time() - mtime_ < delay:  # Was modified recently, sleep more
                time.sleep(delay)
                continue
            else:
                with self.view as view:
                    view['view'] = None
                    view['mtime'] = None

                fly_view = view_
                fly_window = fly_view.window()

                auto_check_enabled = Settings.PLUGIN.enable_auto_check
                auto_lint_enabled = Settings.PLUGIN.enable_auto_lint
                sublime.set_timeout(lambda: fly_window.run_command('sublime_haskell_scan_contents'), 0)

                if auto_check_enabled and auto_lint_enabled:
                    sublime.set_timeout(lambda: fly_window.run_command('sublime_haskell_check_and_lint', {'fly': True}), 0)
                elif auto_check_enabled:
                    sublime.set_timeout(lambda: fly_window.run_command('sublime_haskell_check', {'fly': True}), 0)
                elif auto_lint_enabled:
                    sublime.set_timeout(lambda: fly_window.run_command('sublime_haskell_lint', {'fly': True}), 0)
