"""The main API for the client-side of hsdev.
"""

import json
import pprint
import queue
import random
import socket
import sys
import threading
import time
import traceback

import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.internals.logging as Logging

class HsDevConnection(threading.Thread):
    def __init__(self, sock, rcvr_queue, ident):
        super().__init__(name="receiver {0}".format(ident))
        self.socket = sock
        self.stop_event = threading.Event()
        self.rcvr_queue = rcvr_queue
        self.send_queue = queue.Queue()
        self.conn_ident = ident

    def run(self):
        '''Read from the connection's socket until encoutering a newline. The remaining part of the input stream
        is stashed in self.part for the next time the connection reads a request.
        '''

        self.stop_event.clear()
        threading.Thread(name='hsdev sender {0}'.format(self.conn_ident), target=self.sender).start()

        partial = ''
        while not self.stop_event.is_set():
            try:
                # Note: We could have read a lot from the socket, which could have resulted in multiple request responses
                # being read (this can happen when the 'scan' command sends status updates):
                pre, sep, post = partial.partition('\n')
                if len(sep) > 0:
                    req_resp = pre
                    partial = post
                else:
                    req_resp = partial
                    complete_req = False
                    while not complete_req:
                        streaminp = self.socket.recv(10240).decode('utf-8')
                        pre, sep, post = streaminp.partition('\n')
                        req_resp = req_resp + pre
                        if len(sep) > 0:
                            complete_req = True
                            partial = post

                if self.rcvr_queue is not None:
                    # close might have been called between the time that recv() returned and we try to queue up
                    # the reply.
                    self.rcvr_queue.put(json.loads(req_resp))
            except OSError:
                self.stop_event.set()

    def sender(self):
        while not self.stop_event.is_set():
            try:
                # Block, but timeout, so that we can exit the loop gracefully
                request = self.send_queue.get(True, 6.0)
                self.socket.sendall(request)
                self.send_queue.task_done()
            except queue.Empty:
                pass
            except OSError:
                self.stop_event.set()

    def send_request(self, request):
        msg = json.dumps(request, separators=(',', ':')) + '\n'
        self.send_queue.put(msg.encode('utf-8'))

    def close(self):
        try:
            self.stop_event.set()

            if self.socket is not None:
                try:
                    # User might still see socket errorrs, but at last we tried to tell hsdev to exit.
                    msg = json.dumps({'command': 'exit', 'id': '999', 'no-file': True}, separators=(',', ':')) + '\n'
                    self.socket.sendall(msg.encode('utf-8'))
                except OSError:
                    pass
                finally:
                    self.socket.close()
                    self.socket = None
            self.rcvr_queue = None
            self.send_queue = None
        except OSError:
            pass

    def __del__(self):
        # Paranoia.
        self.close()

class HsDevClient(object):
    '''A client connection from SublimeHaskell to the hsdev server. It implements a primitive socket pool to maximize
    SublimeHaskell responsiveness (concurrency).
    '''
    # Number of times we try to connect to the hsdev server
    CONNECT_TRIES = 10
    # Delay between attempts if not successful
    CONNECT_DELAY = 2.0
    # Initial number of available sockets
    N_SOCKETS = 4

    def __init__(self, backend_mgr):
        # Primitive socket pool:
        self.backend_mgr = backend_mgr
        self.socket_pool = []
        # Request receiver thread:
        self.rcvr_thread = None
        self.rcvr_event = threading.Event()
        self.request_q = queue.Queue()
        self.request_map = LockedObject.LockedObject({})
        self.serial_lock = threading.RLock()
        self.request_serial = 1

    def __del__(self):
        self.close()

    # Socket functions

    def connect(self, host, port):
        for i in range(0, self.N_SOCKETS):
            sock = self.create_connection(i, host, port)
            if sock is not None:
                hsconn = HsDevConnection(sock, self.request_q, i)
                self.socket_pool.insert(i, hsconn)
            else:
                # Something went wrong, terminate all connnections:
                for sock in self.socket_pool:
                    try:
                        sock.close()
                    except OSError:
                        pass
                self.socket_pool = []
                self.request_q = None
                return False

        # We were successful, start all of the socket threads...
        for sock in self.socket_pool:
            sock.start()

        self.rcvr_thread = threading.Thread(target=self.receiver)
        self.rcvr_thread.start()

        Logging.log('Connections established to \'hsdev\' server.', Logging.LOG_INFO)
        return True

    ## Rethink this in terms of multiple threads and a priority queue?
    def create_connection(self, idx, host, port):
        for retry in range(1, self.CONNECT_TRIES):
            try:
                Logging.log('[pool {0}]: connecting to hsdev server (attempt {1})...'.format(idx, retry), Logging.LOG_INFO)
                # Use 'localhost' instead of the IP dot-quad for systems (and they exist) that are solely
                # IPv6. Makes this code friendlier to IPv4 and IPv6 hybrid systems.
                return socket.create_connection((host, port))

            except IOError:
                # Captures all of the socket exceptions:
                Logging.log('[pool {0}]: Failed to connect to hsdev server:'.format(idx), Logging.LOG_WARNING)
                print(traceback.format_exc())
                time.sleep(self.CONNECT_DELAY)

        return None

    def close(self):
        self.rcvr_event.set()
        self.request_q = None

        for sock in self.socket_pool:
            try:
                sock.close()
            except OSError:
                pass
        self.socket_pool = []

        if self.rcvr_thread is not None:
            while self.rcvr_thread.is_alive():
                self.rcvr_thread.join(2.000)
            self.rcvr_thread = None

    def is_connected(self):
        return len(self.socket_pool) > 0

    def setup_receive_callbacks(self, ident, command, on_response, on_notify, on_error):
        with self.request_map as requests:
            requests[ident] = HsCallback.HsDevCallbacks(ident, command, on_response, on_notify, on_error)

    def verify_connected(self):
        return self.is_connected()

    def connection_lost(self, func_name, reason):
        self.close()
        Logging.log('{0}: connection to hsdev lost: {1}'.format(func_name, reason), Logging.LOG_ERROR)

        # send error to callbacks
        with self.request_map as requests:
            for on_msg in requests.values():
                on_msg.on_error('connection lost', {})
            requests.clear()

        self.request_serial = 1

    def receiver(self):
        self.rcvr_event.clear()
        while not self.rcvr_event.is_set() and self.verify_connected():
            try:
                # Ensure that get() will return when the backend is shutting down.
                resp = self.request_q.get(True, 5.0)
                if Settings.BACKEND.all_messages or Settings.BACKEND.recv_messages:
                    print(u'HsDevClient.receiver resp:')
                    pprint.pprint(resp)

                try:
                    with self.request_map as requests:
                        resp_id = resp.get('id')
                        if resp_id is None and 'request' in resp:
                            # Error without an id, id is in the 'request' key's content.
                            orig_req = json.loads(resp['request'])
                            resp_id = orig_req.get('id')

                        if resp_id is not None:
                            callbacks = requests.get(resp_id)
                            if callbacks is not None:
                                finished_request = False
                                if 'notify' in resp:
                                    if Settings.BACKEND.callbacks:
                                        print('id {0}: notify callback'.format(resp_id))
                                    callbacks.call_notify(resp['notify'])
                                if 'error' in resp:
                                    if Settings.BACKEND.callbacks:
                                        print('id {0}: error callback'.format(resp_id))
                                    err = resp.pop("error")
                                    callbacks.call_error(err, resp)
                                    finished_request = True
                                if 'result' in resp:
                                    if Settings.BACKEND.callbacks:
                                        print('id {0}: result callback'.format(resp_id))
                                    callbacks.call_response(resp['result'])
                                    finished_request = True

                                if finished_request:
                                    del requests[resp_id]
                            elif Logging.is_log_level(Logging.LOG_WARNING):
                                print('HsDevClient.receiver: no callbacks for request {0}'.format(resp_id))
                                pprint.pprint(resp)
                        elif Logging.is_log_level(Logging.LOG_ERROR):
                            print('HsDevClient.receiver: request w/o id')
                            pprint.pprint(resp)
                finally:
                    if self.request_q is not None:
                        self.request_q.task_done()
            except queue.Empty:
                pass

    def call(self, command, opts, on_response, on_notify, on_error, wait, timeout):
        # log
        if not self.verify_connected():
            return None if wait else False

        opts = opts or {}
        wait_receive = threading.Event() if wait else None
        result_dict = {}

        def client_call_response(resp):
            result_dict['result'] = resp
            HsCallback.call_callback(on_response, resp)
            if wait_receive is not None:
                wait_receive.set()

        def client_call_error(exc, details):
            HsCallback.call_callback(on_error, exc, details)
            if wait_receive is not None:
                wait_receive.set()

        with self.serial_lock:
            req_serial = str(self.request_serial)
            self.request_serial = self.request_serial + 1

        if wait or on_response or on_notify or on_error:
            args_cmd = 'hsdev {0}'.format(command)
            self.setup_receive_callbacks(req_serial, args_cmd, client_call_response, on_notify, client_call_error)

        opts.update({'no-file': True})
        opts.update({'id': req_serial, 'command': command})

        call_cmd = u'HsDevClient.call[{0}] cmd \'{1}\' opts\n{2}'.format(req_serial, command, pprint.pformat(opts))
        if Settings.BACKEND.all_messages or Settings.BACKEND.send_messages:
            print(call_cmd)

        try:
            # Randomly choose a connection from the pool -- even if we end up having a stuck sender, this should minimize
            # the probability of a complete hang.
            random.choice(self.socket_pool).send_request(opts)

            if wait:
                wait_receive.wait(timeout)
                if not wait_receive.is_set():
                    Logging.log('HsDevClient.call: wait_receive event timed out for id {0}'.format(req_serial),
                                Logging.LOG_ERROR)
                    # Delete the request; result_dict will still have nothing in it (presumably)
                    with self.request_map as requests:
                        del requests[req_serial]

            if Settings.BACKEND.socket_pool:
                with self.request_map as request_map:
                    print('id {0} request_map {1} queue {2}'.format(req_serial, len(request_map), self.request_q.qsize()))

            return result_dict.get('result') if wait else True

        except OSError:
            self.connection_lost('call', sys.exc_info()[1])
            self.backend_mgr.lost_connection()
            return False
