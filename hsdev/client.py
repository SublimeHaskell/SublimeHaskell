"""
"""

import json
import socket
import sys
import threading
import time
import traceback

import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.internals.logging as Logging

# hsdev client
# see for functions with command decorator for hsdev api
class HsDevClient(object):
    # Number of times we try to connect to the hsdev server
    CONNECT_TRIES = 10
    # Delay between attempts if not successful
    CONNECT_DELAY = 2.0

    def __init__(self):
        self.socket = None
        self.listener = None
        self.request_map = LockedObject.LockedObject({})
        self.request_serial = 1

        self.part = ''

        self.on_connected = None
        self.on_disconnected = None
        self.on_reconnect = None

    def __del__(self):
        self.close()

    # Socket functions

    def connect(self, host, port):
        try:
            for retry in range(1, HsDevClient.CONNECT_TRIES):
                Logging.log('connecting to hsdev server (attempt {0})...'.format(retry), Logging.LOG_INFO)

                # Use 'localhost' instead of the IP dot-quad for systems (and they exist) that are solely
                # IPv6. Makes this code friendlier to IPv4 and IPv6 hybrid systems.
                self.socket = socket.create_connection((host, port))
                self.listener = threading.Thread(target=self.listen)
                self.listener.start()

                Logging.log('Connection established to \'hsdev\' server.', Logging.LOG_INFO)
                HsCallback.call_callback(self.on_connected, name='HsDev.on_connected')
                return True

        except OSError:
            # Captures all of the socket exceptions:
            Logging.log('Failed to connect to hsdev server:', Logging.LOG_WARNING)
            print(traceback.format_exc())
            time.sleep(HsDevClient.CONNECT_DELAY)

        return False

    def close(self):
        self.socket.close()
        self.socket = None

    def is_connected(self):
        return self.socket is not None

    def on_receive(self, ident, command, on_response=None, on_notify=None, on_error=None):
        with self.request_map as requests:
            requests[ident] = HsCallback.HsDevCallbacks(ident, command, on_response, on_notify, on_error)

    def verify_connected(self):
        return self.is_connected()

    def connection_lost(self, func_name, reason):
        self.close()
        Logging.log('{0}: connection to hsdev lost: {1}'.format(func_name, reason), Logging.LOG_ERROR)
        HsCallback.call_callback(self.on_disconnected, name='HsDev.on_disconnected')

        # send error to callbacks
        with self.request_map as requests:
            for on_msg in requests.values():
                on_msg.on_error('connection lost', {})
            requests.clear()

        self.request_serial = 1
        self.part = ''


    def listen(self):
        while self.verify_connected():
            try:
                resp = json.loads(self.get_response())
                if 'id' in resp:
                    callbacks = None
                    with self.request_map as requests:
                        if resp['id'] in requests:
                            callbacks = requests[resp['id']]
                    if callbacks:
                        if 'notify' in resp:
                            callbacks.call_notify(resp['notify'])
                        if 'error' in resp:
                            err = resp.pop("error")
                            callbacks.call_error(err, resp)
                            with self.request_map as requests:
                                requests.pop(resp['id'])
                        if 'result' in resp:
                            callbacks.call_response(resp['result'])
                            with self.request_map as requests:
                                requests.pop(resp['id'])
            except IOError:
                self.connection_lost('listen', sys.exc_info()[1])
                return

    def get_response(self):
        while '\n' not in self.part:
            self.part = self.part + self.socket.recv(4096).decode('utf-8')
        (pre, _, post) = self.part.partition('\n')
        self.part = post
        return pre

    def call(self, command, opts, on_response, on_notify, on_error, wait, timeout):
        # log
        args_cmd = 'hsdev {0}'.format(command)
        call_cmd = 'hsdev {0} with {1}'.format(command, opts)

        if not self.verify_connected():
            return None if wait else False

        try:
            opts = opts or {}
            wait_receive = threading.Event() if wait else None
            result_dict = {}

            def on_response_(resp):
                result_dict['result'] = resp
                HsCallback.call_callback(on_response, resp)
                if wait_receive:
                    wait_receive.set()

            def on_error_(exc, details):
                HsCallback.call_callback(on_error, exc, details)
                if wait_receive:
                    wait_receive.set()

            req_serial = str(self.request_serial)
            self.request_serial = self.request_serial + 1

            if wait or on_response or on_notify or on_error:
                self.on_receive(req_serial, args_cmd, on_response_, on_notify, on_error_)

            opts.update({'no-file': True})
            opts.update({'id': req_serial, 'command': command})
            msg = json.dumps(opts, separators=(',', ':'))

            # The first sendall sends the request -- if the socket encounters an error, it isn't reported
            # immediately. It is actually reported on the next send() or sendall() [nutshell: TCP acks the
            # first sendall(), but if the remote side closes the socket, you have to wait until the next
            # TCP ack to know that that's been done.] OTOH, the first sendall() can also fail...
            self.socket.sendall(msg.encode('utf-8'))
            self.socket.sendall('\n'.encode('utf-8'))
            Logging.log(call_cmd, Logging.LOG_TRACE)

            if wait:
                wait_receive.wait(timeout)
                return result_dict.get('result')
            else:
                return True

        except OSError:
            Logging.log('{0} fails with exception, see traceback in console window.'.format(call_cmd), Logging.LOG_ERROR)
            print(traceback.format_exc())
            self.connection_lost('call', sys.exc_info()[1])
            return False
