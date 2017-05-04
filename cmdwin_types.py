'''Command window types for SublimeHaskell.

SublimeText has several different "command" classes. The two important ones are `sublime_plugin.TextCommand` and
`sublime_plugin.WindowCommand`. These two classes have `is_enabled` and `is_visible` methods, which SublimeHaskell
overrides to enable commands if the source code is Haskell source.
'''
import sublime_plugin

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.backend_mgr as BackendMgr
import SublimeHaskell.internals.logging as Logging


class SublimeHaskellWindowCommand(sublime_plugin.WindowCommand):
    '''Enable window commands if the source is Haskell source.
    '''
    def __init__(self, view):
        super().__init__(view)

    def is_enabled(self):
        return Common.is_enabled_haskell_command(None, False)

    def is_visible(self):
        return Common.is_enabled_haskell_command(None, False)


class SublimeHaskellTextCommand(sublime_plugin.TextCommand):
    '''Enable text commands if the source is Haskell source.
    '''
    def __init__(self, view):
        super().__init__(view)

    def is_enabled(self):
        return Common.is_enabled_haskell_command(self.view, False)

    def is_visible(self):
        return Common.is_enabled_haskell_command(self.view, False)

class HsDevWindowCommand(SublimeHaskellWindowCommand):
    def __init__(self, view):
        super().__init__(view)

    def is_enabled(self):
        live = BackendMgr.is_live_backend()
        cmd_enabled = super().is_enabled()
        Logging.log('HsDevWindowCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_enabled),
                    Logging.LOG_DEBUG)
        return live and cmd_enabled

    def is_visible(self):
        live = BackendMgr.is_live_backend()
        cmd_visible = super().is_visible()
        Logging.log('HsDevWindowCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_visible),
                    Logging.LOG_DEBUG)
        return live and cmd_visible


class HsDevTextCommand(SublimeHaskellTextCommand):
    '''Enable text commands only if the backend is alive and the source in the view is Haskell source.
    '''
    def __init__(self, view):
        super().__init__(view)

    def is_enabled(self):
        live = BackendMgr.is_live_backend()
        cmd_enabled = super().is_enabled()
        Logging.log('HsDevTextCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_enabled),
                    Logging.LOG_DEBUG)
        return live and cmd_enabled

    def is_visible(self):
        live = BackendMgr.is_live_backend()
        cmd_visible = super().is_visible()
        Logging.log('HsDevTextCommand.is_enabled: is_live_backend {0}, super {1}'.format(live, cmd_visible),
                    Logging.LOG_DEBUG)
        return live and cmd_visible
