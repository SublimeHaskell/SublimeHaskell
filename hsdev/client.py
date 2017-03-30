

import io
import json
import re
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
import SublimeHaskell.internals.proc_helper as ProcHelper


def cmd(name_, opts_, on_result=lambda r: r):
    return (name_, opts_, on_result)


def file_contents(files, contents):
    return [{'file': f, 'contents': None} for f in files] + [{'file': f, 'contents': cts} for f, cts in contents.items()]

# hsdev client
# see for functions with command decorator for hsdev api
class HsDev(object):
    def __init__(self, port):
        self.port = port
        self.connecting = threading.Event()
        self.connected = threading.Event()
        self.socket = None
        self.listener = None
        self.hsdev_socket = None
        self.hsdev_address = None
        self.autoconnect = True
        self.map = LockedObject.LockedObject({})
        self.id = 1

        self.connect_fun = None

        self.part = ''

        self.on_connected = None
        self.on_disconnected = None
        self.on_reconnect = None

    def __del__(self):
        self.close()

    # Autoconnect
    def set_reconnect_function(self, f):
        if self.connect_fun is None:
            self.connect_fun = f

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
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        for n in range(0, tries):
            try:
                Logging.log('connecting to hsdev server ({0})...'.format(n), Logging.LOG_INFO)
                self.socket.connect(('127.0.0.1', self.port))
                self.hsdev_socket = self.socket
                self.hsdev_address = '127.0.0.1'
                self.set_connected()
                self.listener = threading.Thread(target=self.listen)
                self.listener.start()
                Logging.log('connected to hsdev server', Logging.LOG_INFO)
                HsCallback.call_callback(self.on_connected, name='HsDev.on_connected')
                return True
            except:
                Logging.log('Failed to connect to hsdev server:', Logging.LOG_WARNING)
                print(traceback.format_exc())
                time.sleep(delay)

        return False

    @HsDecorator.reconnect_function
    def connect_async(self, tries=10, delay=1.0):
        thread = threading.Thread(target=self.connect,
                                  kwargs={'tries': tries, 'delay': delay, 'just_connect': True})
        thread.start()

    def wait(self, timeout=None):
        return self.connected.wait(timeout)

    def close(self):
        if self.is_unconnected():
            return
        self.connected.clear()
        if self.hsdev_socket:
            self.hsdev_socket.close()
            self.hsdev_socket = None
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

    def on_receive(self, id, command, on_response=None, on_notify=None, on_error=None):
        with self.map as m:
            m[id] = HsCallback.HsDevCallbacks(id, command, on_response, on_notify, on_error)

    def verify_connected(self):
        if self.is_connected():
            return True
        else:
            self.connection_lost('verify_connected', 'no connection')
            return self.is_connected()

    def connection_lost(self, fn, e):
        if self.is_unconnected():
            return
        self.close()
        Logging.log('{0}: connection to hsdev lost: {1}'.format(fn, e), Logging.LOG_ERROR)
        HsCallback.call_callback(self.on_disconnected, name='HsDev.on_disconnected')

        # send error to callbacks
        with self.map as m:
            for on_msg in m.values():
                on_msg.on_error('connection lost', {})
            m.clear()

        self.id = 1
        self.part = ''

        if self.autoconnect:
            self.reconnect()

    def call(self, command, opts={}, on_response=None, on_notify=None, on_error=None, wait=False, timeout=None, id=None):
        # log
        args_cmd = 'hsdev {0}'.format(command)
        call_cmd = 'hsdev {0} with {1}'.format(command, opts)

        if not self.verify_connected():
            return None if wait else False

        try:
            wait_receive = threading.Event() if wait else None

            x = {}

            def on_response_(r):
                x['result'] = r
                HsCallback.call_callback(on_response, r)
                if wait_receive:
                    wait_receive.set()

            def on_error_(e, ds):
                HsCallback.call_callback(on_error, e, ds)
                if wait_receive:
                    wait_receive.set()

            if wait or on_response or on_notify or on_error:
                if id is None:
                    id = str(self.id)
                    self.id = self.id + 1
                self.on_receive(id, args_cmd, on_response_, on_notify, on_error_)

            opts.update({'no-file': True})
            opts.update({'id': id, 'command': command})
            msg = json.dumps(opts, separators=(',', ':'))

            # Seems, that first sendall doesn't throw error on closed socket
            # So we just call it twice
            # It's hackish, but I haven't found easy solution
            self.hsdev_socket.sendall(msg.encode('utf-8'))
            self.hsdev_socket.sendall('\n'.encode('utf-8'))
            Logging.log(call_cmd, Logging.LOG_TRACE)

            if wait:
                wait_receive.wait(timeout)
                return x.get('result')

            return True
        except:
            Logging.log('{0} fails with exception, see traceback in console window.'.format(call_cmd), Logging.LOG_ERROR)
            print(traceback.format_exc())
            self.connection_lost('call', sys.exc_info[1])
            return False

    def listen(self):
        while self.verify_connected():
            try:
                resp = json.loads(self.get_response())
                if 'id' in resp:
                    callbacks = None
                    with self.map as m:
                        if resp['id'] in m:
                            callbacks = m[resp['id']]
                    if callbacks:
                        if 'notify' in resp:
                            callbacks.call_notify(resp['notify'])
                        if 'error' in resp:
                            err = resp.pop("error")
                            callbacks.call_error(err, resp)
                            with self.map as m:
                                m.pop(resp['id'])
                        if 'result' in resp:
                            callbacks.call_response(resp['result'])
                            with self.map as m:
                                m.pop(resp['id'])
            except Exception as e:
                self.connection_lost('listen', e)
                return

    def get_response(self):
        while '\n' not in self.part:
            self.part = self.part + self.socket.recv(65536).decode('utf-8')
        (r, _, post) = self.part.partition('\n')
        self.part = post
        return r

    # Commands

    @HsDecorator.command
    def link(self, hold=False, **kwargs):
        return cmd('link', {'hold': hold})

    @HsDecorator.command
    def ping(self):
        return cmd('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'))

    @HsDecorator.async_command
    def scan(self, cabal=False, sandboxes=[], projects=[], files=[], paths=[], ghc=[], contents={}, docs=False, infer=False):
        return cmd('scan', {'projects': projects,
                            'cabal': cabal,
                            'sandboxes': sandboxes,
                            'files': file_contents(files, contents),
                            'paths': paths,
                            'ghc-opts': ghc,
                            'docs': docs,
                            'infer': infer})

    @HsDecorator.async_command
    def docs(self, projects=[], files=[], modules=[]):
        return cmd('docs', {'projects': projects,
                            'files': files,
                            'modules': modules})

    @HsDecorator.async_command
    def infer(self, projects=[], files=[], modules=[]):
        return cmd('infer', {'projects': projects,
                             'files': files,
                             'modules': modules})

    @HsDecorator.async_list_command
    def remove(self, cabal=False, sandboxes=[], projects=[], files=[], packages=[]):
        return cmd('remove', {'projects': projects,
                              'cabal': cabal,
                              'sandboxes': sandboxes,
                              'files': files,
                              'packages': packages})

    @HsDecorator.command
    def remove_all(self):
        return cmd('remove-all', {})

    @HsDecorator.list_command
    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, db=None, package=None,
                     source=False, standalone=False):
        fs = []
        if project:
            fs.append({'project': project})
        if file:
            fs.append({'file': file})
        if module:
            fs.append({'module': module})
        if deps:
            fs.append({'deps': deps})
        if sandbox:
            fs.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            fs.append({'cabal': 'cabal'})
        if db:
            fs.append({'db': ResultParse.encode_package_db(db)})
        if package:
            fs.append({'package': package})
        if source:
            fs.append('sourced')
        if standalone:
            fs.append('standalone')

        return cmd('modules', {'filters': fs}, ResultParse.parse_modules_brief)

    @HsDecorator.list_command
    def list_packages(self):
        return cmd('packages', {})

    @HsDecorator.list_command
    def list_projects(self):
        return cmd('projects', {})

    @HsDecorator.list_command
    def symbol(self, input="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, db=None, package=None, source=False, standalone=False, locals=False):
        # search_type is one of: exact, prefix, infix, suffix, regex
        q = {'input': input, 'type': search_type}

        fs = []
        if project:
            fs.append({'project': project})
        if file:
            fs.append({'file': file})
        if module:
            fs.append({'module': module})
        if deps:
            fs.append({'deps': deps})
        if sandbox:
            fs.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            fs.append({'cabal': 'cabal'})
        if db:
            fs.append({'db': ResultParse.encode_package_db(db)})
        if package:
            fs.append({'package': package})
        if source:
            fs.append('sourced')
        if standalone:
            fs.append('standalone')

        return cmd('symbol', {'query': q, 'filters': fs, 'locals': locals}, ResultParse.parse_decls)

    @HsDecorator.command
    def module(self, input="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, db=None, package=None, source=False, standalone=False):
        q = {'input': input, 'type': search_type}

        fs = []
        if project:
            fs.append({'project': project})
        if file:
            fs.append({'file': file})
        if module:
            fs.append({'module': module})
        if deps:
            fs.append({'deps': deps})
        if sandbox:
            fs.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            fs.append({'cabal': 'cabal'})
        if db:
            fs.append({'db': ResultParse.encode_package_db(db)})
        if package:
            fs.append({'package': package})
        if source:
            fs.append('sourced')
        if standalone:
            fs.append('standalone')

        return cmd('module', {'query': q, 'filters': fs}, ResultParse.parse_modules)

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
    def scope_modules(self, file, input='', search_type='prefix'):
        return cmd('scope modules', {'query': {'input': input, 'type': search_type}, 'file': file},
                   ResultParse.parse_modules_brief)

    @HsDecorator.list_command
    def scope(self, file, input='', search_type='prefix', global_scope=False):
        return cmd('scope',
                   {'query': {'input': input,
                              'type': search_type
                             },
                    'global': global_scope,
                    'file': file
                   }, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def complete(self, input, file, wide=False):
        return cmd('complete', {'prefix': input, 'wide': wide, 'file': file}, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def hayoo(self, query, page=None, pages=None):
        return cmd('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1}, ResultParse.parse_decls)

    @HsDecorator.list_command
    def cabal_list(self, packages):
        cmd('cabal list', {'packages': packages},
            lambda r: [ResultParse.parse_cabal_package(s) for s in r] if r else None)

    @HsDecorator.list_command
    def lint(self, files=[], contents={}, hlint=[]):
        return cmd('lint', {'files': file_contents(files, contents), 'hlint-opts': hlint})

    @HsDecorator.list_command
    def check(self, files=[], contents={}, ghc=[]):
        return cmd('check', {'files': file_contents(files, contents), 'ghc-opts': ghc})

    @HsDecorator.list_command
    def check_lint(self, files=[], contents={}, ghc=[], hlint=[]):
        return cmd('check-lint', {'files': file_contents(files, contents), 'ghc-opts': ghc, 'hlint-opts': hlint})

    @HsDecorator.list_command
    def types(self, files=[], contents={}, ghc=[]):
        return cmd('types', {'files': file_contents(files, contents), 'ghc-opts': ghc})

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
    def autofix_fix(self, messages, rest=[], pure=False):
        return cmd('autofix fix', {'messages': messages, 'rest': rest, 'pure': pure}, ResultParse.parse_corrections)

    @HsDecorator.list_command
    def ghc_eval(self, exprs, file=None, source=None):
        f = None
        if file is not None:
            f = {'file': f, 'contents': source}
        return cmd('ghc eval', {'exprs': exprs, 'file': f})

    @HsDecorator.command
    def exit(self):
        return cmd('exit', {})
