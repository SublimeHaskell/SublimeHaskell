
import time

import SublimeHaskell.internals.logging as Logging

def call_callback(callback_fn, *args, **kwargs):
    name = kwargs.get('name')
    if name:
        del kwargs['name']
    if callback_fn is not None:
        # Logging.log('call_callback invoking {0}'.format(callback_fn), Logging.LOG_DEBUG)
        callback_fn(*args, **kwargs)


class HsDevCallbacks(object):
    def __init__(self, call_id, command, on_response=None, on_notify=None, on_error=None):
        self.ident = call_id
        self.command = command
        self.start_time = time.clock()
        self.on_response = on_response
        self.on_notify = on_notify
        self.on_error = on_error

    def time(self):
        return time.clock() - self.start_time if self.start_time is not None else None

    def log_time(self):
        Logging.log('{0}: {1} seconds'.format(self.command, self.time()), Logging.LOG_TRACE)

    def call_response(self, resp):
        self.log_time()
        call_callback(self.on_response, resp)

    def call_notify(self, notify_msg):
        call_callback(self.on_notify, notify_msg)

    def call_error(self, err, details):
        self.log_time()
        comb_details = ', '.join(['{0}: {1}'.format(k, v) for k, v in details.items()])
        Logging.log('{0} returns error: {1}\n{2}'.format(self.command, err, comb_details), Logging.LOG_ERROR)
        call_callback(self.on_error, err, details)
