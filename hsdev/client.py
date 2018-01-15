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

import SublimeHaskell.internals.atomics as Atomics
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils

def debug_send():
    return Settings.COMPONENT_DEBUG.all_messages or Settings.COMPONENT_DEBUG.send_messages

def debug_recv():
    return Settings.COMPONENT_DEBUG.all_messages or Settings.COMPONENT_DEBUG.recv_messages

class HsDevConnection(object):
    MAX_SOCKET_READ = 10240
    '''Byte stream length that the client receiver will read from a socket at one time.
    '''

    def __init__(self, sock, client, ident):
        super().__init__()
        self.socket = sock
        self.stop_event = threading.Event()
        self.client = client
        self.send_queue = queue.Queue()
        self.conn_ident = ident
        self.rcvr_thread = threading.Thread(name="hsdev recvr {0}".format(self.conn_ident), target=self.hsconn_receiver)
        self.send_thread = threading.Thread(name='hsdev sender {0}'.format(self.conn_ident), target=self.hsconn_sender)

    def start_connection(self):
        self.stop_event.clear()
        self.rcvr_thread.start()
        self.send_thread.start()

    def hsconn_receiver(self):
        '''Read from the connection's socket until encoutering a newline. The remaining part of the input stream
        is stashed in self.part for the next time the connection reads a request.
        '''

        req_remain = ''
        while not self.stop_event.is_set() and self.socket is not None:
            # Note: We could have read a lot from the socket, which could have resulted in multiple request responses
            # being read (this can happen when the 'scan' command sends status updates):
            pre, sep, post = self.read_decoded_req().partition('\n')
            pre = ''.join([req_remain, pre])
            while sep:
                resp = json.loads(pre)
                self.client.dispatch_response(resp)
                (pre, sep, post) = post.partition('\n')
            req_remain = pre

    def read_decoded_req(self):
        raw_req = bytearray()
        while self.socket is not None and not self.stop_event.is_set():
            try:
                raw_req.extend(self.socket.recv(self.MAX_SOCKET_READ))
                return raw_req.decode('utf-8').replace('\r\n', '\n').replace('\r', '\n')
            except UnicodeDecodeError:
                # Couldn't decode the string, so continue reading from the socket, accumulating
                # into stream_inp until utf-8 decoding succeeds.
                if debug_recv():
                    print(u'{0}.read_decoded_req: UnicodeDecodeError'.format(type(self).__name__))
            except (OSError, socket.gaierror):
                # Socket problem. Just bail.
                self.stop_event.set()
                break
        return ''

    def hsconn_sender(self):
        while not self.stop_event.is_set():
            try:
                # Block, but timeout, so that we can exit the loop gracefully
                request = self.send_queue.get(True, 6.0)
                if self.socket is not None:
                    # Socket got closed and set to None in another thread...
                    self.socket.sendall(request)
                if self.send_queue is not None:
                    self.send_queue.task_done()
            except queue.Empty:
                pass
            except OSError:
                self.stop_event.set()

    def send_request(self, request):
        if self.send_queue is not None:
            msg = json.dumps(request, separators=(',', ':')) + '\n'
            self.send_queue.put(msg.encode('utf-8'))

    def close(self):
        try:
            self.stop_event.set()

            if self.socket is not None:
                self.socket.close()
                self.socket = None
            if self.rcvr_thread is not None and self.rcvr_thread.is_alive():
                self.rcvr_thread.join()
            if self.send_thread is not None and self.send_thread.is_alive():
                self.send_thread.join()
            self.client = None
            self.send_queue = None
            self.rcvr_thread = None
            self.send_thread = None
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
        self.rcvr_event = threading.Event()
        self._request_map = Atomics.AtomicDuck()

    def __del__(self):
        self.close()

    @property
    def request_map(self):
        return self._request_map


    # Socket functions

    def connect(self, host, port):
        for i in range(0, self.N_SOCKETS):
            sock = self.create_connection(i, host, port)
            if sock is not None:
                hsconn = HsDevConnection(sock, self, i)
                self.socket_pool.insert(i, hsconn)
            else:
                # Something went wrong, terminate all connnections:
                for sock in self.socket_pool:
                    try:
                        sock.close()
                    except OSError:
                        pass
                self.socket_pool = []
                return False

        # We were successful, start all of the socket threads, make them available...
        for sock in self.socket_pool:
            sock.start_connection()

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

            except (socket.gaierror, IOError):
                # Captures all of the socket exceptions:
                msg = '[pool {0}]: Failed to connect to hsdev {1}:{2}:'.format(idx, host, port)
                Logging.log(msg, Logging.LOG_ERROR)
                print(traceback.format_exc())
                time.sleep(self.CONNECT_DELAY)

        return None

    def close(self):
        self.rcvr_event.set()

        for sock in self.socket_pool:
            try:
                sock.close()
            except OSError:
                pass
        del self.socket_pool[:]

    def is_connected(self):
        return len(self.socket_pool) > 0

    def verify_connected(self):
        return self.is_connected()

    def connection_lost(self, func_name, reason):
        self.close()
        Logging.log('{0}: connection to hsdev lost: {1}'.format(func_name, reason), Logging.LOG_ERROR)

        # send error to callbacks
        with self.request_map as requests:
            for (callbacks, _, _) in requests.values():
                callbacks.call_on_error('connection lost', {})
            requests.clear()

    def dispatch_response(self, resp):
        resp_id = resp.get('id')
        if not resp_id and 'request' in resp:
            # Error without an id, id is in the 'request' key's content.
            orig_req = json.loads(resp['request'])
            resp_id = orig_req.get('id')
        if resp_id:
            with self.request_map as requests:
                reqdata = requests.get(resp_id)
                if reqdata:
                    callbacks, wait_event, _ = reqdata

                    # Unconditionally call the notify callback:
                    if 'notify' in resp:
                        if Settings.COMPONENT_DEBUG.callbacks:
                            print('id {0}: notify callback'.format(resp_id))
                        callbacks.call_notify(resp['notify'])

                    if 'result' in resp or 'error' in resp:
                        if wait_event:
                            requests[resp_id] = (callbacks, wait_event, resp)
                            # The wait-er calls callbacks, cleans up after the request.
                            wait_event.set()
                        else:
                            # Enqueue for async receiver: Put the (resp_id, resp, reqdata) tuple onto the queue
                            del requests[resp_id]
                            if 'result' in resp:
                                Utils.run_async('result {0}'.format(resp_id), callbacks.call_response, resp['result'])
                            elif 'error' in resp:
                                err = resp.pop("error")
                                Utils.run_async('error {0}'.format(resp_id), callbacks.call_error, err, resp)
                else:
                    msg = 'HsDevClient.receiver: request data expected for {0}, none found.'.format(resp_id)
                    Logging.log(msg, Logging.LOG_WARNING)
        elif Logging.is_log_level(Logging.LOG_ERROR):
            print('HsDevClient.receiver: request w/o id\n{0}'.format(pprint.pformat(resp)))

    def call(self, command, opts, callbacks, wait, timeout):
        if not self.verify_connected():
            return None if wait else False

        opts = opts or {}
        opts.update({'no-file': True, 'id': callbacks.ident, 'command': command})

        wait_receive = threading.Event() if wait else None

        if debug_recv():
            def client_call_error(exc, details):
                print('{0}.client_call_error: exc {1} details {2}'.format(type(self).__name__, exc, details))

            callbacks.on_error.append(client_call_error)

        with self.request_map as requests:
            requests[callbacks.ident] = (callbacks, wait_receive, None)

        if debug_send():
            print(u'HsDevClient.call[{0}] cmd \'{1}\' opts\n{2}'.format(callbacks.ident, command, pprint.pformat(opts)))

        try:
            # Randomly choose a connection from the pool -- even if we end up having a stuck sender, this should minimize
            # the probability of a complete hang.
            random.choice(self.socket_pool).send_request(opts)

            retval = True
            if wait:
                if debug_send():
                    print(u'HsDevClient.call: waiting to receive, timeout = {0}.'.format(timeout))
                wait_receive.wait(timeout)
                if debug_send():
                    print(u'HsDevClient.call: done waiting.')

                with self.request_map as requests:
                    if wait_receive.is_set():
                        callbacks, _, resp = requests[callbacks.ident]
                        del requests[callbacks.ident]

                        retval = resp
                        if 'result' in resp:
                            retval = callbacks.call_response(resp['result'])
                        elif 'error' in resp:
                            err = resp.pop("error")
                            retval = callbacks.call_error(err, resp)

                        if Settings.COMPONENT_DEBUG.callbacks:
                            print('HsDevClient.call: wait {0} result {1}'.format(callbacks.ident, retval))
                    else:
                        req = pprint.pformat(opts)
                        errmsg = 'HsDevClient.call: wait_receive event timed out for id {0}\n{1}'.format(callbacks.ident, req)
                        Logging.log(errmsg, Logging.LOG_ERROR)

            if Settings.COMPONENT_DEBUG.socket_pool:
                with self.request_map as request_map:
                    print('id {0} request_map {1}'.format(callbacks.ident, len(request_map)))

            return retval

        except OSError:
            self.connection_lost('call', sys.exc_info()[1])
            self.backend_mgr.lost_connection()
            return False
