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
import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.hsdev.decorators as HsDecorator
import SublimeHaskell.hsdev.result_parse as ResultParse
import SublimeHaskell.internals.logging as Logging


def cmd(name_, opts_, on_result=lambda r: r):
    return (name_, opts_, on_result)


def files_and_contents(files, contents):
    return [{'file': f, 'contents': None} for f in files] + [{'file': f, 'contents': cts} for f, cts in contents.items()]

# hsdev client
# see for functions with command decorator for hsdev api
class HsDev(object):
    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.connecting = threading.Event()
        self.connected = threading.Event()
        self.socket = None
        self.listener = None
        self.autoconnect = True
        self.request_map = LockedObject.LockedObject({})
        self.request_serial = 1

        self.connect_fun = None

        self.part = ''

        self.on_connected = None
        self.on_disconnected = None
        self.on_reconnect = None

    def __del__(self):
        self.close()

    # Autoconnect
    def set_reconnect_function(self, reconnect_fn):
        if self.connect_fun is None:
            self.connect_fun = reconnect_fn

    def reconnect(self):
        if self.connect_fun is not None:
            Logging.log('Reconnecting to hsdev...', Logging.LOG_INFO)
            HsCallback.call_callback(self.on_reconnect, name='HsDev.on_reconnect')
            self.connect_fun()
        else:
            Logging.log('No reconnect function')

    # Socket functions

    @HsDecorator.connect_function
    @HsDecorator.reconnect_function
    def connect(self, tries=10, delay=1.0):
        try:
            for retry in range(0, tries):
                Logging.log('connecting to hsdev server (attempt {0})...'.format(retry), Logging.LOG_INFO)

                # Use 'localhost' instead of the IP dot-quad for systems (and they exist) that are solely
                # IPv6. Makes this code friendlier to IPv4 and IPv6 hybrid systems.
                self.socket = socket.create_connection((self.host, self.port))
                self.set_connected()
                self.listener = threading.Thread(target=self.listen)
                self.listener.start()

                Logging.log('connected to hsdev server', Logging.LOG_INFO)
                HsCallback.call_callback(self.on_connected, name='HsDev.on_connected')
                return True

        except OSError:
            # Captures all of the socket exceptions:
            Logging.log('Failed to connect to hsdev server:', Logging.LOG_WARNING)
            print(traceback.format_exc())
            time.sleep(delay)

        return False

    def set_on_connected(self, conn_fn):
        self.on_connected = conn_fn

    @HsDecorator.reconnect_function
    def connect_async(self, tries=10, delay=1.0):
        threading.Thread(target=self.connect, kwargs={'tries': tries, 'delay': delay, 'just_connect': True}).start()

    def wait(self, timeout=None):
        return self.connected.wait(timeout)

    def close(self):
        if not self.is_unconnected():
            self.connected.clear()
            self.socket.close()

    def is_connecting(self):
        return self.connecting.is_set()

    def is_connected(self):
        return self.connected.is_set()

    def is_unconnected(self):
        return (not self.is_connecting()) and (not self.is_connected())

    def set_unconnected(self):
        if self.connecting.is_set():
            self.connecting.clear()
        if self.connected.is_set():
            self.connected.clear()

    def set_connecting(self):
        self.set_unconnected()
        self.connecting.set()

    def set_connected(self):
        if self.is_connecting():
            self.connected.set()
            self.connecting.clear()
        else:
            Logging.log('HsDev.set_connected called while not in connecting state', Logging.LOG_DEBUG)

    def on_receive(self, ident, command, on_response=None, on_notify=None, on_error=None):
        with self.request_map as requests:
            requests[ident] = HsCallback.HsDevCallbacks(ident, command, on_response, on_notify, on_error)

    def verify_connected(self):
        if self.is_connected():
            return True
        else:
            self.connection_lost('verify_connected', 'no connection')
            return self.is_connected()

    def connection_lost(self, func_name, reason):
        if self.is_unconnected():
            return
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

        if self.autoconnect:
            self.reconnect()

    def call(self, command, opts=None, on_response=None, on_notify=None, on_error=None, wait=False, timeout=None):
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

            # Seems, that first sendall doesn't throw error on closed socket
            # So we just call it twice
            # It's hackish, but I haven't found easy solution
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
            self.part = self.part + self.socket.recv(65536).decode('utf-8')
        (pre, _, post) = self.part.partition('\n')
        self.part = post
        return pre

    # Commands

    @HsDecorator.command
    def link(self, hold=False):
        retval = cmd('link', {'hold': hold})
        Logging.log("link command sent, result: {0}".format(retval), Logging.LOG_DEBUG)
        return retval

    @HsDecorator.command
    def ping(self):
        return cmd('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'))

    @HsDecorator.async_command
    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False):
        return cmd('scan', {'projects': projects or [],
                            'cabal': cabal,
                            'sandboxes': sandboxes or [],
                            'files': files_and_contents(files or [], contents or {}),
                            'paths': paths or [],
                            'ghc-opts': ghc or [],
                            'docs': docs,
                            'infer': infer})

    @HsDecorator.async_command
    def docs(self, projects=None, files=None, modules=None):
        return cmd('docs', {'projects': projects or [],
                            'files': files or [],
                            'modules': modules or []})

    @HsDecorator.async_command
    def infer(self, projects=None, files=None, modules=None):
        return cmd('infer', {'projects': projects or [],
                             'files': files or [],
                             'modules': modules or []})

    @HsDecorator.async_list_command
    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None):
        return cmd('remove', {'projects': projects or [],
                              'cabal': cabal,
                              'sandboxes': sandboxes or [],
                              'files': files or [],
                              'packages': packages or []})

    @HsDecorator.command
    def remove_all(self):
        return cmd('remove-all', {})

    @HsDecorator.list_command
    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False):
        filters = []
        if project:
            filters.append({'project': project})
        if file:
            filters.append({'file': file})
        if module:
            filters.append({'module': module})
        if deps:
            filters.append({'deps': deps})
        if sandbox:
            filters.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            filters.append({'cabal': 'cabal'})
        if symdb:
            filters.append({'db': ResultParse.encode_package_db(symdb)})
        if package:
            filters.append({'package': package})
        if source:
            filters.append('sourced')
        if standalone:
            filters.append('standalone')

        return cmd('modules', {'filters': filters}, ResultParse.parse_modules_brief)

    @HsDecorator.list_command
    def list_packages(self):
        return cmd('packages', {})

    @HsDecorator.list_command
    def list_projects(self):
        return cmd('projects', {})

    @HsDecorator.list_command
    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False):
        # search_type is one of: exact, prefix, infix, suffix, regex
        query = {'input': lookup, 'type': search_type}

        filters = []
        if project:
            filters.append({'project': project})
        if file:
            filters.append({'file': file})
        if module:
            filters.append({'module': module})
        if deps:
            filters.append({'deps': deps})
        if sandbox:
            filters.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            filters.append({'cabal': 'cabal'})
        if symdb:
            filters.append({'db': ResultParse.encode_package_db(symdb)})
        if package:
            filters.append({'package': package})
        if source:
            filters.append('sourced')
        if standalone:
            filters.append('standalone')

        return cmd('symbol', {'query': query, 'filters': filters, 'locals': local_names}, ResultParse.parse_decls)

    @HsDecorator.command
    def module(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False):
        query = {'input': lookup, 'type': search_type}

        filters = []
        if project:
            filters.append({'project': project})
        if file:
            filters.append({'file': file})
        if module:
            filters.append({'module': module})
        if deps:
            filters.append({'deps': deps})
        if sandbox:
            filters.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            filters.append({'cabal': 'cabal'})
        if symdb:
            filters.append({'db': ResultParse.encode_package_db(symdb)})
        if package:
            filters.append({'package': package})
        if source:
            filters.append('sourced')
        if standalone:
            filters.append('standalone')

        return cmd('module', {'query': query, 'filters': filters}, ResultParse.parse_modules)

    @HsDecorator.command
    def resolve(self, file, exports=False):
        return cmd('resolve', {'file': file, 'exports': exports}, ResultParse.parse_module)

    @HsDecorator.command
    def project(self, project=None, path=None):
        return cmd('project', {'name': project} if project else {'path': path})

    @HsDecorator.command
    def sandbox(self, path):
        return cmd('sandbox', {'path': path})

    @HsDecorator.list_command
    def lookup(self, name, file):
        return cmd('lookup', {'name': name, 'file': file}, ResultParse.parse_decls)

    @HsDecorator.list_command
    def whois(self, name, file):
        return cmd('whois', {'name': name, 'file': file}, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def scope_modules(self, file, lookup='', search_type='prefix'):
        return cmd('scope modules', {'query': {'input': lookup, 'type': search_type}, 'file': file},
                   ResultParse.parse_modules_brief)

    @HsDecorator.list_command
    def scope(self, file, lookup='', search_type='prefix', global_scope=False):
        return cmd('scope',
                   {'query': {'input': lookup,
                              'type': search_type
                             },
                    'global': global_scope,
                    'file': file
                   }, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def complete(self, lookup, file, wide=False):
        return cmd('complete', {'prefix': lookup, 'wide': wide, 'file': file}, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def hayoo(self, query, page=None, pages=None):
        return cmd('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1}, ResultParse.parse_decls)

    @HsDecorator.list_command
    def cabal_list(self, packages):
        cmd('cabal list', {'packages': packages},
            lambda r: [ResultParse.parse_cabal_package(s) for s in r] if r else None)

    @HsDecorator.list_command
    def lint(self, files=None, contents=None, hlint=None):
        retval = cmd('lint', {'files': files_and_contents(files or [], contents or {}), 'hlint-opts': hlint or []})
        Logging.log('hsdev.lint: retval\n{0}'.format(pprint.pformat(retval)))
        return retval

    @HsDecorator.list_command
    def check(self, files=None, contents=None, ghc=None):
        return cmd('check', {'files': files_and_contents(files or [], contents or {}), 'ghc-opts': ghc or []})

    @HsDecorator.list_command
    def check_lint(self, files=None, contents=None, ghc=None, hlint=None):
        return cmd('check-lint', {'files': files_and_contents(files or [], contents or {}), 'ghc-opts': ghc or [],
                                  'hlint-opts': hlint or []})

    @HsDecorator.list_command
    def types(self, files=None, contents=None, ghc=None):
        return cmd('types', {'files': files_and_contents(files or [], contents or {}), 'ghc-opts': ghc or []})

    @HsDecorator.command
    def langs(self):
        return cmd('langs', {})

    @HsDecorator.command
    def flags(self):
        return cmd('flags', {})

    @HsDecorator.list_command
    def autofix_show(self, messages):
        return cmd('autofix show', {'messages': messages}, ResultParse.parse_corrections)

    @HsDecorator.list_command
    def autofix_fix(self, messages, rest=None, pure=False):
        return cmd('autofix fix', {'messages': messages, 'rest': rest or [], 'pure': pure}, ResultParse.parse_corrections)

    @HsDecorator.list_command
    def ghc_eval(self, exprs, file=None, source=None):
        the_file = None
        if file is not None:
            the_file = {'file': the_file, 'contents': source}
        return cmd('ghc eval', {'exprs': exprs, 'file': the_file})

    @HsDecorator.command
    def exit(self):
        return cmd('exit', {})
