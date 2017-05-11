"""The main API for the client-side of hsdev.
"""

import json
import pprint
import socket
import sys
import threading
import time
import traceback

import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.hsdev.iowait as iowait

class HsDevConnection(object):
    def __init__(self, sock):
        super().__init__()
        self.socket = sock
        self.partial = ''

    def read_request(self):
        '''Read from the connection's socket until encoutering a newline. The remaining part of the input stream
        is stashed in self.part for the next time the connection reads a request.
        '''
        # Note: We could have read a lot from the socket, which could have resulted in multiple request responses
        # being read (this can happen when the 'scan' command sends status updates):
        pre, sep, post = self.partial.partition('\n')
        if len(sep) > 0:
            req_resp = pre
            self.partial = post
        else:
            req_resp = self.partial
            complete_req = False
            while not complete_req:
                streaminp = self.socket.recv(10240).decode('utf-8')
                pre, sep, post = streaminp.partition('\n')
                req_resp = req_resp + pre
                if len(sep) > 0:
                    complete_req = True
                    self.partial = post

        return json.loads(req_resp)

    def have_more(self):
        '''Is there more to read, i.e., was a partially read request stored?
        '''
        return len(self.partial) > 0

    def close(self):
        try:
            self.socket.close()
            self.socket = None
            self.partial = ''
        except OSError:
            pass

    def __del__(self):
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
        self.socket_lock = threading.RLock()
        self.socket_pool = {}
        self.socket_used = {}
        self.iowait_q = iowait.IOWait()
        # Request receiver thread:
        self.rcvr_thread = None
        self.rcvr_event = threading.Event()
        self.request_map = LockedObject.LockedObject({})
        self.request_serial = 1

    def __del__(self):
        self.close()

    # Socket functions

    def connect(self, host, port):
        with self.socket_lock:
            for i in range(0, HsDevClient.N_SOCKETS):
                sock = self.create_connection(i, host, port)
                if sock is not None:
                    self.socket_pool[sock] = HsDevConnection(sock)
                    self.iowait_q.watch(sock, True, False)
                else:
                    # Something went wrong, terminate all connnections:
                    for sock in self.socket_pool:
                        try:
                            sock.close()
                        except OSError:
                            pass
                    self.socket_pool = {}
                    self.socket_used = {}
                    self.iowait_q.clear()
                    return False

            self.rcvr_thread = threading.Thread(target=self.receiver)
            self.rcvr_thread.start()

            Logging.log('Connections established to \'hsdev\' server.', Logging.LOG_INFO)
            return True

    def create_connection(self, idx, host, port):
        for retry in range(1, HsDevClient.CONNECT_TRIES):
            try:
                Logging.log('[pool {0}]: connecting to hsdev server (attempt {1})...'.format(idx, retry), Logging.LOG_INFO)
                # Use 'localhost' instead of the IP dot-quad for systems (and they exist) that are solely
                # IPv6. Makes this code friendlier to IPv4 and IPv6 hybrid systems.
                return socket.create_connection((host, port))

            except IOError:
                # Captures all of the socket exceptions:
                Logging.log('[pool {0}]: Failed to connect to hsdev server:'.format(idx), Logging.LOG_WARNING)
                print(traceback.format_exc())
                time.sleep(HsDevClient.CONNECT_DELAY)

        return None

    def close(self):
        self.rcvr_event.set()
        self.iowait_q.clear()

        with self.socket_lock:
            for sock in self.socket_pool:
                try:
                    sock.close()
                except OSError:
                    pass
            for sock in self.socket_used:
                try:
                    sock.close()
                except OSError:
                    pass
            self.socket_pool = {}
            self.socket_used = {}

        if self.rcvr_thread is not None:
            while self.rcvr_thread.is_alive():
                self.rcvr_thread.join(0.500)
            self.rcvr_thread = None

    def is_connected(self):
        with self.socket_lock:
            return self.socket_pool or self.socket_used

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
                if Settings.BACKEND.iowaits:
                    print(u'HsDevClient.reciever: entering iowait loop.')

                avail_inps = self.iowait_q.wait()
                for inp in avail_inps:
                    if Settings.BACKEND.iowaits:
                        print(u'HsDevClient.reciever: iowait completed.')

                    with self.socket_lock:
                        conn = self.socket_used.get(inp.fileobj) or self.socket_pool.get(inp.fileobj)

                    if conn is not None:
                        self.get_response(conn)
                        # Continue to process incomplete responses -- it means we haven't read enough.
                        while conn.have_more():
                            self.get_response(conn)
                    else:
                        Logging.log('HsDevClient.receiver: no corresponding connection {0}'.format(inp.fileobj),
                                    Logging.LOG_ERROR)
            except OSError:
                self.connection_lost('receiver', sys.exc_info()[1])
                self.backend_mgr.lost_connection()
                return

            except KeyError:
                if not self.rcvr_event.is_set():
                    self.connection_lost('receiver', sys.exc_info()[1])
                    self.backend_mgr.lost_connection()
                # Otherwise, the thread has already been shut down and we should just return.
                return

    def get_response(self, hsdev_conn):
        resp = hsdev_conn.read_request()
        if Settings.BACKEND.all_messages or Settings.BACKEND.recv_messages:
            print(u'HsDevClient.receiver resp:')
            pprint.pprint(resp)

        if 'id' in resp:
            callbacks = None
            with self.request_map as requests:
                resp_id = resp.get('id')
                if resp_id is not None:
                    callbacks = requests.get(resp_id)
                    if callbacks is not None:
                        if 'notify' in resp:
                            callbacks.call_notify(resp['notify'])
                        if 'error' in resp:
                            err = resp.pop("error")
                            callbacks.call_error(err, resp)
                            requests.pop(resp['id'])
                        if 'result' in resp:
                            callbacks.call_response(resp['result'])
                            requests.pop(resp['id'])
        else:
            Logging.log('HsDevClient.receover: received response without an id', Logging.LOG_ERROR)

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

        req_serial = str(self.request_serial)
        self.request_serial = self.request_serial + 1

        if wait or on_response or on_notify or on_error:
            args_cmd = 'hsdev {0}'.format(command)
            self.setup_receive_callbacks(req_serial, args_cmd, client_call_response, on_notify, client_call_error)

        opts.update({'no-file': True})
        opts.update({'id': req_serial, 'command': command})
        msg = json.dumps(opts, separators=(',', ':')) + '\n'

        call_cmd = u'HsDevClient.call[{0}] cmd \'{1}\' opts\n{2}'.format(req_serial, command, pprint.pformat(opts))
        if Settings.BACKEND.all_messages or Settings.BACKEND.send_messages:
            print(call_cmd)

        try:
            sock = None
            conn = None

            with self.socket_lock:
                # Randomly remove a socket from the socket pool
                try:
                    sock, conn = self.socket_pool.popitem()
                    self.socket_used[sock] = conn
                except KeyError:
                    pass

            if sock is not None:
                sock.sendall(msg.encode('utf-8'))
                if wait:
                    wait_receive.wait(timeout)
                # Put the socket back into the pool
                with self.socket_lock:
                    del self.socket_used[sock]
                    self.socket_pool[sock] = conn

                if Settings.BACKEND.socket_pool:
                    print('socket_pool {0} socket_used {1}'.format(len(self.socket_pool), len(self.socket_used)))
                return result_dict.get('result') if wait else True
            else:
                Logging.log('HsDevClient.call: Socket pool exhausted?', Logging.LOG_ERROR)

        except OSError:
            self.connection_lost('call', sys.exc_info()[1])
            self.backend_mgr.lost_connection()
            return False
