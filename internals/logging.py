import sublime

if int(sublime.version()) < 3000:
    import internals.settings as Settings
else:
    import SublimeHaskell.internals.settings as Settings

# Logging primitives
LOG_ERROR = 1
LOG_WARNING = 2
LOG_INFO = 3
LOG_DEBUG = 4
LOG_TRACE = 5


def log(message, level=LOG_INFO):
    log_level = Settings.get_setting_async('log', LOG_INFO)
    if log_level >= level:
        print(u'Sublime Haskell: {0}'.format(message))
