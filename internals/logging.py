import SublimeHaskell.internals.settings as Settings

# Logging primitives
LOG_ERROR = 1
LOG_WARNING = 2
LOG_INFO = 3
LOG_DEBUG = 4
LOG_TRACE = 5


def log(message, level=LOG_INFO):
    log_level = Settings.PLUGIN.log or LOG_INFO
    if log_level >= level:
        emit(message)

def emit(message):
    print(u'Sublime Haskell: {0}'.format(message))

def current_log_level():
    return Settings.PLUGIN.log

def is_log_level(level):
    return Settings.PLUGIN.log >= level
