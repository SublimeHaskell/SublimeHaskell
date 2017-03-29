
import time
import traceback

import SublimeHaskell.internals.logging as Logging

def call_callback(fn, *args, **kwargs):
    name = kwargs.get('name')
    if name:
        del kwargs['name']
    try:
        if fn is not None:
            fn(*args, **kwargs)
    except:
        Logging.log('callback \'{0}\' throws exception, see console window traceback'.format(name or '<unnamed>'),
                    Logging.LOG_ERROR)
        print(traceback.format_exc())


class HsDevCallbacks(object):
    def __init__(self, id, command, on_response=None, on_notify=None, on_error=None):
        self.id = id
        self.command = command
        self.start_time = time.clock()
        self.on_response = on_response
        self.on_notify = on_notify
        self.on_error = on_error

    def time(self):
        return time.clock() - self.start_time if self.start_time is not None else None

    def log_time(self):
        Logging.log('{0}: {1} seconds'.format(self.command, self.time()), Logging.LOG_TRACE)

    def call_response(self, r):
        self.log_time()
        call_callback(self.on_response, r)

    def call_notify(self, n):
        call_callback(self.on_notify, n)

    def call_error(self, e, ds):
        self.log_time()
        details = ', '.join(['{0}: {1}'.format(k, v) for k, v in ds.items()])
        Logging.log('{0} returns error: {1}\n{2}'.format(self.command, e, details), Logging.LOG_ERROR)
        call_callback(self.on_error, e, ds)
