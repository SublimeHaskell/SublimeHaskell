'''Command window types for SublimeHaskell.

SublimeText has several different "command" classes. The two important ones are `sublime_plugin.TextCommand` and
`sublime_plugin.WindowCommand`. These two classes have `is_enabled` and `is_visible` methods, which SublimeHaskell
overrides to enable commands if the source code is Haskell source.
'''
import sublime_plugin

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.backend_mgr as BackendMgr


class SublimeHaskellWindowCommand(sublime_plugin.WindowCommand):
    '''Enable window commands if the source is Haskell source.
    '''
    ## Uncomment if instance attributes added.
    # def __init__(self, view):
    #     super().__init__(view)

    def is_enabled(self):
        return Common.is_enabled_haskell_command(None, False)

    def is_visible(self):
        return Common.is_enabled_haskell_command(None, False)


class SublimeHaskellTextCommand(sublime_plugin.TextCommand):
    '''Enable text commands if the source is Haskell source.
    '''
    ## Uncomment if instance attributes added.
    # def __init__(self, view):
    #     super().__init__(view)

    def is_enabled(self):
        return Common.is_enabled_haskell_command(self.view, False)

    def is_visible(self):
        return Common.is_enabled_haskell_command(self.view, False)

class BackendWindowCommand(SublimeHaskellWindowCommand):
    ## Uncomment if instance attributes added.
    # def __init__(self, win):
    #     super().__init__(win)

    def is_enabled(self):
        live = BackendMgr.is_live_backend()
        inspector_busy = BackendMgr.inspector_busy()
        cmd_enabled = super().is_enabled()
        # Logging.log('BackendWindowCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_enabled),
        #             Logging.LOG_DEBUG)
        return live and not inspector_busy and cmd_enabled

    def is_visible(self):
        live = BackendMgr.is_live_backend()
        cmd_visible = super().is_visible()
        # Logging.log('BackendWindowCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_visible),
        #             Logging.LOG_DEBUG)
        return live and cmd_visible


class BackendTextCommand(SublimeHaskellTextCommand):
    '''Enable text commands only if the backend is alive and the source in the view is Haskell source.
    '''

    ## Uncomment if instance attributes added.
    # def __init__(self, view):
    #     super().__init__(view)

    def is_enabled(self):
        live = BackendMgr.is_live_backend()
        inspector_busy = BackendMgr.inspector_busy()
        cmd_enabled = super().is_enabled()
        # Logging.log('BackendTextCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_enabled),
        #             Logging.LOG_DEBUG)
        return live and not inspector_busy and cmd_enabled

    def is_visible(self):
        live = BackendMgr.is_live_backend()
        cmd_visible = super().is_visible()
        # Logging.log('BackendTextCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_visible),
        #             Logging.LOG_DEBUG)
        return live and cmd_visible


class HaskellSourceBackendTextCommand(BackendTextCommand):
    '''Enable text commands only if the backend is alive and the source in the view is Haskell source.
    '''

    ## Uncomment if instance attributes added.
    # def __init__(self, view):
    #     super().__init__(view)

    def is_enabled(self):
        return Common.view_is_haskell_source(self.view) and super().is_enabled()

    def is_visible(self):
        return Common.view_is_haskell_source(self.view) and super().is_visible()
