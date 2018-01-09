
import time

import SublimeHaskell.internals.logging as Logging

# def call_callback(callback_fn, *args, **kwargs):
#     name = kwargs.get('name')
#     if name:
#         del kwargs['name']
#     if callback_fn is not None:
#         callback_fn(*args, **kwargs)


class HsDevCallbacks(object):
    def __init__(self, ident, command, on_response=None, result_convert=None, on_notify=None, on_error=None):
        # Really for debugging
        self._ident = ident
        self.command = command
        self.start_time = time.clock()
        # The actual internals.
        self.on_response = []
        self.result_convert = []
        self.on_notify = []
        self.on_error = []

        if on_response:
            self.on_response.append(on_response)
        if result_convert:
            self.result_convert.append(result_convert)
        if on_notify:
            self.on_notify.append(on_notify)
        if on_error:
            self.on_error.append(on_error)

    @property
    def ident(self):
        return self._ident

    @ident.setter
    def ident(self, value):
        self._ident = str(value)

    def time(self):
        return time.clock() - self.start_time if self.start_time is not None else None

    def log_time(self):
        Logging.log('{0}: {1} seconds'.format(self.command, self.time()), Logging.LOG_TRACE)

    def call_result_convert(self, resp):
        ret_resp = resp
        for resultcvt in reversed(self.result_convert):
            ret_resp = resultcvt(ret_resp)

        return ret_resp

    def add_result_convert(self, result_converter):
        '''Add a response function to the on_response function list.
        '''
        self.result_convert.insert(0, result_converter)

    def inject_result_convert(self, result_converter):
        '''Inject a response function onto the end of the on_response chain of functions
        '''
        self.result_convert.append(result_converter)

    def call_response(self, resp):
        self.log_time()

        resp = self.call_result_convert(resp)
        for resp_func in reversed(self.on_response):
            resp_func(resp)

        return resp

    def add_response(self, response_func):
        '''Add a response function to the on_response function list.
        '''
        self.on_response.insert(0, response_func)

    def inject_response(self, response_func):
        '''Inject a response function onto the end of the on_response chain of functions so that it is invoked before
        any of the subsequent functions.
        '''
        self.on_response.append(response_func)

    def call_notify(self, notify_msg):
        for notify_func in reversed(self.on_notify):
            notify_func(notify_msg)

    def add_notify(self, notify_func):
        '''Add a response function to the on_response function list.
        '''
        self.on_response.insert(0, notify_func)

    def inject_notify(self, notify_func):
        '''Inject a response function onto the end of the on_response chain of functions
        '''
        self.on_response.append(notify_func)

    def call_error(self, err, details):
        self.log_time()
        comb_details = ', '.join(['{0}: {1}'.format(k, v) for k, v in details.items()])
        Logging.log('{0} returns error: {1}\n{2}'.format(self.command, err, comb_details), Logging.LOG_ERROR)

        retval = None
        for err_func in reversed(self.on_error):
            retval = err_func(err, details)

        return retval

    def add_error_handler(self, error_func):
        '''Add a response function to the on_response function list.
        '''
        self.on_error.insert(0, error_func)

    def inject_error_handler(self, error_func):
        '''Inject a response function onto the end of the on_response chain of functions
        '''
        self.on_error.append(error_func)
