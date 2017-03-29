# -*- coding: UTF-8 -*-

from functools import reduce
import io
import json
import os
import re
import sys
import threading
import time
import traceback

import socket
import sublime

import SublimeHaskell.symbols as symbols
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.hsdev_refactor.callback as HsCallback
import SublimeHaskell.hsdev_refactor.decorators as HsDecorator
import SublimeHaskell.hsdev_refactor.result_parse as ResultParse
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.worker as Worker


def concat_args(args):
    def cat(x, y):
        (px, ex) = x
        (py, ey) = y
        return (px or py, (ex if px else []) + (ey if py else []))
    return reduce(cat, args, (True, []))[1]


def hsdev_version():
    retval = None
    try:
        exit_code, out, err = ProcHelper.ProcHelper.run_process(['hsdev', 'version'])
        if exit_code == 0:
            m = re.match(r'(?P<major>\d+)\.(?P<minor>\d+)\.(?P<revision>\d+)\.(?P<build>\d+)', out)
            if m:
                major = int(m.group('major'))
                minor = int(m.group('minor'))
                revision = int(m.group('revision'))
                build = int(m.group('build'))
                retval = [major, minor, revision, build]
    except:
        Logging.log('Could not get hsdev version, see console window traceback', Logging.LOG_ERROR)
        print(traceback.format_exc())
    finally:
        return retval


def show_version(ver):
    return '.'.join(map(lambda i: str(i), ver))


def check_version(ver, minimal, maximal):
    if ver is None:
        return False
    if ver < minimal or (maximal and ver >= maximal):
        return False
    return True


def format_error_details(ds):
    return ', '.join(['{}: {}'.format(k, v) for k, v in ds.items()])


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
        HsCallback.call_callback(self.on_response, r)

    def call_notify(self, n):
        HsCallback.call_callback(self.on_notify, n)

    def call_error(self, e, ds):
        self.log_time()
        Logging.log('{0} returns error: {1}, {2}'.format(self.command, e, format_error_details(ds)), Logging.LOG_ERROR)
        HsCallback.call_callback(self.on_error, e, ds)


# hsdev client
# see for functions with command decorator for hsdev api
class HsDev(object):
    def __init__(self, port=4567):
        self.port = port
        self.connecting = threading.Event()
        self.connected = threading.Event()
        self.socket = None
        self.listener = None
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

    # Create server process
    @staticmethod
    def create_server(port=4567, cache=None, log_file=None, log_config=None):
        cmd = concat_args([
            (True, ["hsdev", "run"]),
            (port, ["--port", str(port)]),
            (cache, ["--cache", cache]),
            (log_file, ["--log", log_file]),
            (log_config, ["--log-config", log_config])])

        Logging.log('Starting hsdev server', Logging.LOG_INFO)
        p = ProcHelper.ProcHelper(cmd)
        if p.process is None:
            Logging.log('Failed to create hsdev process', Logging.LOG_ERROR)
            return None

        # Use TextIOWrapper here because it combines decoding with newline handling,
        # which means less to maintain.
        p.process.stdout = io.TextIOWrapper(p.process.stdout, 'utf-8')
        p.process.stderr = io.TextIOWrapper(p.process.stderr, 'utf-8')

        while True:
            output = p.process.stdout.readline()
            m = re.match(r'^.*?hsdev> Server started at port (?P<port>\d+)$', output)
            if m:
                Logging.log('hsdev server started at port {0}'.format(m.group('port')))
                return p.process

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
            except Exception:
                Logging.log('failed to connect to hsdev server ({0})'.format(n), Logging.LOG_WARNING)
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
            m[id] = HsDevCallbacks(id, command, on_response, on_notify, on_error)

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
            self.connection_lost('call', e)
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
        return HsDecorator.cmd('link', {'hold': hold})

    @HsDecorator.command
    def ping(self):
        return HsDecorator.cmd('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'))

    @HsDecorator.async_command
    def scan(self, cabal=False, sandboxes=[], projects=[], files=[], paths=[], ghc=[], contents={}, docs=False, infer=False):
        return HsDecorator.cmd('scan', {
            'projects': projects,
            'cabal': cabal,
            'sandboxes': sandboxes,
            'files': [{'file': f, 'contents': None} for f in files] + \
                     [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'paths': paths,
            'ghc-opts': ghc,
            'docs': docs,
            'infer': infer})

    @HsDecorator.async_command
    def docs(self, projects=[], files=[], modules=[]):
        return HsDecorator.cmd('docs', {
            'projects': projects,
            'files': files,
            'modules': modules})

    @HsDecorator.async_command
    def infer(self, projects=[], files=[], modules=[]):
        return HsDecorator.cmd('infer', {
            'projects': projects,
            'files': files,
            'modules': modules})

    @HsDecorator.async_list_command
    def remove(self, cabal=False, sandboxes=[], projects=[], files=[], packages=[]):
        return HsDecorator.cmd('remove', {
            'projects': projects,
            'cabal': cabal,
            'sandboxes': sandboxes,
            'files': files,
            'packages': packages})

    @HsDecorator.command
    def remove_all(self):
        return HsDecorator.cmd('remove-all', {})

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

        return HsDecorator.cmd('modules', {'filters': fs}, ResultParse.parse_modules_brief)

    @HsDecorator.list_command
    def list_packages(self):
        return HsDecorator.cmd('packages', {})

    @HsDecorator.list_command
    def list_projects(self):
        return HsDecorator.cmd('projects', {})

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

        return HsDecorator.cmd('symbol', {'query': q, 'filters': fs, 'locals': locals}, ResultParse.parse_decls)

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

        return HsDecorator.cmd('module', {'query': q, 'filters': fs}, ResultParse.parse_modules)

    @HsDecorator.command
    def resolve(self, file, exports=False):
        return HsDecorator.cmd('resolve', {'file': file, 'exports': exports}, ResultParse.parse_module)

    @HsDecorator.command
    def project(self, project=None, path=None):
        return HsDecorator.cmd('project', {'name': project} if project else {'path': path})

    @HsDecorator.command
    def sandbox(self, path):
        return HsDecorator.cmd('sandbox', {'path': path})

    @HsDecorator.list_command
    def lookup(self, name, file):
        return HsDecorator.cmd('lookup', {'name': name, 'file': file}, ResultParse.parse_decls)

    @HsDecorator.list_command
    def whois(self, name, file):
        return HsDecorator.cmd('whois', {'name': name, 'file': file}, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def scope_modules(self, file, input='', search_type='prefix'):
        return HsDecorator.cmd('scope modules', {'query': {'input': input, 'type': search_type}, 'file': file},
                               ResultParse.parse_modules_brief)

    @HsDecorator.list_command
    def scope(self, file, input='', search_type='prefix', global_scope=False):
        return HsDecorator.cmd('scope',
                               {'query': {'input': input
                                          , 'type': search_type
                                         }
                                , 'global': global_scope
                                , 'file': file
                               }, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def complete(self, input, file, wide=False):
        return HsDecorator.cmd('complete', {'prefix': input, 'wide': wide, 'file': file}, ResultParse.parse_declarations)

    @HsDecorator.list_command
    def hayoo(self, query, page=None, pages=None):
        return HsDecorator.cmd('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1}, ResultParse.parse_decls)

    @HsDecorator.list_command
    def cabal_list(self, packages):
        HsDecorator.cmd('cabal list', {'packages': packages},
                        lambda r: [ResultParse.parse_cabal_package(s) for s in r] if r else None)

    @HsDecorator.list_command
    def lint(self, files=[], contents={}, hlint=[]):
        return HsDecorator.cmd('lint', {
            'files': [{'file': f, 'contents': None} for f in files] + \
                     [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'hlint-opts': hlint})

    @HsDecorator.list_command
    def check(self, files=[], contents={}, ghc=[]):
        return HsDecorator.cmd('check', {
            'files': [{'file': f, 'contents': None} for f in files] + \
                     [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'ghc-opts': ghc})

    @HsDecorator.list_command
    def check_lint(self, files=[], contents={}, ghc=[], hlint=[]):
        return HsDecorator.cmd('check-lint', {
            'files': [{'file': f, 'contents': None} for f in files] + \
                     [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'ghc-opts': ghc,
            'hlint-opts': hlint})

    @HsDecorator.list_command
    def types(self, files=[], contents={}, ghc=[]):
        return HsDecorator.cmd('types', {
            'files': [{'file': f, 'contents': None} for f in files] + \
                     [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'ghc-opts': ghc})

    @HsDecorator.command
    def langs(self):
        return HsDecorator.cmd('langs', {})

    @HsDecorator.command
    def flags(self):
        return HsDecorator.cmd('flags', {})

    @HsDecorator.list_command
    def autofix_show(self, messages):
        return HsDecorator.cmd('autofix show', {'messages': messages}, ResultParse.parse_corrections)

    @HsDecorator.list_command
    def autofix_fix(self, messages, rest=[], pure=False):
        return HsDecorator.cmd('autofix fix', {'messages': messages, 'rest': rest, 'pure': pure}, ResultParse.parse_corrections)

    @HsDecorator.list_command
    def ghc_eval(self, exprs, file=None, source=None):
        f = None
        if file is not None:
            f = {'file': f, 'contents': source}
        return HsDecorator.cmd('ghc eval', {'exprs': exprs, 'file': f})

    @HsDecorator.command
    def exit(self):
        return HsDecorator.cmd('exit', {})


def wait_result(fn, *args, **kwargs):
    wait_receive = threading.Event()
    x = {'result': None}

    on_resp = kwargs.get('on_response')
    on_err = kwargs.get('on_error')

    def wait_response(r):
        x['result'] = r
        if on_resp:
            on_resp(r)
        wait_receive.set()

    def wait_error(e, ds):
        Logging.log('hsdev call fails with: {0}, {1}'.format(e, format_error_details(ds)))
        if on_err:
            on_err(e, ds)
        wait_receive.set()

    tm = kwargs.pop('timeout', 0.1)

    kwargs['on_response'] = wait_response
    kwargs['on_error'] = wait_error

    fn(*args, **kwargs)

    wait_receive.wait(tm)
    return x['result']


# hsdev server process with auto-restart
class HsDevProcess(threading.Thread):
    def __init__(self, port=4567, cache=None, log_file=None, log_config=None):
        super().__init__()
        self.process = None
        self.drain_stdout = None
        self.drain_stderr = None
        self.on_start = None
        self.on_exit = None
        self.stop_event = threading.Event()
        self.create_event = threading.Event()
        self.port = port
        self.cache = cache
        self.log_file = log_file
        self.log_config = log_config

    def run(self):
        while True:
            self.create_event.wait()
            self.create_event.clear()
            while not self.stop_event.is_set():
                self.process = HsDev.create_server(port=self.port,
                                                   cache=self.cache,
                                                   log_file=self.log_file,
                                                   log_config=self.log_config)
                if not self.process:
                    Logging.log('failed to create hsdev process', Logging.LOG_ERROR)
                    self.stop_event.set()
                else:
                    self.drain_stdout = OutputCollector.DescriptorDrain('hsdev stdout', self.process.stdout)
                    self.drain_stderr = OutputCollector.DescriptorDrain('hsdev stderr', self.process.stderr)
                    self.drain_stdout.start()
                    self.drain_stderr.start()
                    HsCallback.call_callback(self.on_start, name='HsDevProcess.on_start')
                self.process.wait()
                if self.drain_stdout:
                    self.drain_stdout.stop()
                if self.drain_stderr:
                    self.drain_stderr.stop()
                HsCallback.call_callback(self.on_exit, name='HsDevProcess.on_exit')
            self.stop_event.clear()

    def active(self):
        return self.process.poll() is None

    def inactive(self):
        return self.process.poll() is not None

    def create(self):
        self.create_event.set()

    def stop(self):
        self.stop_event.set()


### HACK ALERT: If agent, client and client_back are already present in the
### module's globals, don't redefine them. Otherwise, you will end up with
### multiple instances of hsdev if the plugin is reloaded.

# global hsdev agent
if 'agent' not in globals():
    agent = None
else:
    agent = globals()['agent']
# global hsdev agent's hsdev client for command tasks
if 'client' not in globals():
    client = None
else:
    client = globals()['client']
# global hsdev agent's hsdev client for background tasks (commonly scanning)
if 'client_back' not in globals():
    client_back = None
else:
    client_back = globals()['client_back']


# Show scan progress in status bar
class scan_status(object):
    def __init__(self, status_message):
        self.status_message = status_message

    def __call__(self, msg):
        statuses = []
        for m in msg:
            p = m['progress']
            statuses.append('{0} ({1}/{2})'.format(m['name'], p['current'], p['total']) if p else m['name'])
        self.status_message.change_message('Inspecting {0}'.format(' / '.join(statuses)))


# Set reinspect event
def dirty(fn):
    def wrapped(self, *args, **kwargs):
        if not hasattr(self, 'dirty_lock'):
            self.dirty_lock = threading.Lock()
        acquired = self.dirty_lock.acquire(blocking=False)
        try:
            return fn(self, *args, **kwargs)
        finally:
            if acquired:
                self.dirty_lock.release()
                self.reinspect_event.set()
    return wrapped


def use_inspect_modules(fn):
    def wrapped(self, *args, **kwargs):
        if Settings.get_setting_async('inspect_modules'):
            return fn(self, *args, **kwargs)
    return wrapped


def agent_connected():
    return agent.is_connected()


# Return default value if hsdev is not enabled/connected
def use_hsdev(def_val=None):
    def wrap(fn):
        def wrapped(*args, **kwargs):
            if Settings.get_setting_async('enable_hsdev') and agent_connected():
                return fn(*args, **kwargs)
            else:
                return def_val
        return wrapped
    return wrap


# hsdev agent
# holds hsdev server process and two clients: for commands and for background tasks
# also automatically reinspects files/paths/etc. when they marked as dirty
class HsDevAgent(threading.Thread):
    sleep_timeout = 60.0  # agent sleeping timeout
    min_ver = [0, 2, 0, 0]  # minimal hsdev version
    max_ver = [0, 2, 3, 0]  # maximal hsdev version

    def __init__(self):
        super().__init__()
        self.daemon = True
        self.cabal_to_load = LockedObject.LockedObject([])
        self.dirty_files = LockedObject.LockedObject([])
        self.dirty_paths = LockedObject.LockedObject([])
        self.hsdev_process = HsDevProcess(cache=os.path.join(Common.sublime_haskell_cache_path(), 'hsdev'),
                                          log_file=os.path.join(Common.sublime_haskell_cache_path(), 'hsdev', 'hsdev.log'),
                                          log_config=Settings.get_setting_async('hsdev_log_config'))
        self.client = HsDev()
        self.client_back = HsDev()

        self.reinspect_event = threading.Event()

    def is_connected(self):
        return self.client.is_connected()

    def start_hsdev(self, start_server=True):
        hsdev_ver = hsdev_version()
        if hsdev_ver is None:
            Common.output_error_async(sublime.active_window(), "\n".join([
                "SublimeHaskell: hsdev executable couldn't be found!",
                "It's used in most features of SublimeHaskell",
                "Check if it's installed and in PATH",
                "If it's not installed, run 'cabal install hsdev' to install hsdev",
                "You may also want to adjust 'add_to_PATH' setting",
                "",
                "To supress this message and disable hsdev set 'enable_hsdev' to false"]))
        elif not check_version(hsdev_ver, HsDevAgent.min_ver, HsDevAgent.max_ver):
            Common.output_error_async(sublime.active_window(), "\n".join([
                "SublimeHaskell: hsdev version is incorrect: {0}".format(show_version(hsdev_ver)),
                "Required version: >= {0} and < {1}".format(show_version(HsDevAgent.min_ver), show_version(HsDevAgent.max_ver)),
                "Update it by running 'cabal update' and 'cabal install hsdev'",
                "",
                "To supress this message and disable hsdev set 'enable_hsdev' to false"]))
        else:
            def start_():
                Logging.log('hsdev process started', Logging.LOG_TRACE)
                self.client.connect_async()
                self.client_back.connect_async()

            def exit_():
                Logging.log('hsdev process exited', Logging.LOG_TRACE)
                self.client.close()
                self.client_back.close()

            def connected_():
                Logging.log('hsdev agent: connected to hsdev', Logging.LOG_TRACE)
                self.client.link()

            def back_connected_():
                Logging.log('hsdev agent: connected to hsdev', Logging.LOG_TRACE)
                self.start_inspect()

            self.client.on_connected = connected_
            self.client_back.on_connected = back_connected_

            self.hsdev_process.on_start = start_
            self.hsdev_process.on_exit = exit_

            self.hsdev_process.start()
            self.hsdev_process.create()

    def stop_hsdev(self):
        self.client.close()
        self.client_back.close()

    def on_hsdev_enabled(self, key, value):
        if key == 'enable_hsdev':
            if value:
                Logging.log("starting hsdev", Logging.LOG_INFO)
                self.hsdev_process.create()
            else:
                Logging.log("stopping hsdev", Logging.LOG_INFO)
                self.hsdev_process.stop()
                self.client.close()
                self.client_back.close()

    def on_inspect_modules_changed(self, key, value):
        if key == 'inspect_modules':
            if value:
                self.mark_all_files()

    def run(self):
        Settings.subscribe_setting('enable_hsdev', self.on_hsdev_enabled)
        Settings.subscribe_setting('inspect_modules', self.on_inspect_modules_changed)

        if Settings.get_setting_async('enable_hsdev'):
            self.start_hsdev()

        while True:
            if Settings.get_setting_async('enable_hsdev') and not self.client.ping():
                Logging.log('hsdev ping: no pong', Logging.LOG_WARNING)

            scan_paths = []
            with self.dirty_paths as dirty_paths:
                scan_paths = dirty_paths[:]
                dirty_paths[:] = []

            files_to_reinspect = []
            with self.dirty_files as dirty_files:
                files_to_reinspect = dirty_files[:]
                dirty_files[:] = []

            projects = []
            files = []

            if len(files_to_reinspect) > 0:
                projects = []
                files = []
                for f in files_to_reinspect:
                    d = Common.get_cabal_project_dir_of_file(f)
                    if d is not None:
                        projects.append(d)
                    else:
                        files.append(f)

            projects = list(set(projects))
            files = list(set(files))

            try:
                self.inspect(paths=scan_paths, projects=projects, files=files)
            except Exception as e:
                Logging.log('HsDevAgent inspect exception: {0}'.format(e))

            load_cabal = []
            with self.cabal_to_load as cabal_to_load:
                load_cabal = cabal_to_load[:]
                cabal_to_load[:] = []

            for c in load_cabal:
                Worker.run_async('inspect cabal {0}'.format(c), self.inspect_cabal, c)

            if files_to_reinspect:
                if Settings.get_setting_async('enable_hdocs'):
                    self.client_back.docs(files=files_to_reinspect)
            self.reinspect_event.wait(HsDevAgent.sleep_timeout)
            self.reinspect_event.clear()

    @dirty
    def force_inspect(self):
        self.reinspect_event.set()

    @dirty
    def start_inspect(self):
        self.mark_cabal()
        self.mark_all_files()

    @dirty
    @use_inspect_modules
    def mark_all_files(self):
        window = sublime.active_window()
        with self.dirty_files as dirty_files:
            dirty_files.extend(list(filter(lambda f: f and f.endswith('.hs'), [v.file_name() for v in window.views()])))
        with self.dirty_paths as dirty_paths:
            dirty_paths.extend(window.folders())

    @dirty
    @use_inspect_modules
    def mark_file_dirty(self, filename):
        if filename is None:
            return
        with self.dirty_files as dirty_files:
            dirty_files.append(filename)

    @dirty
    def mark_cabal(self, cabal_name=None):
        with self.cabal_to_load as cabal_to_load:
            cabal_to_load.append(cabal_name or 'cabal')

    @use_hsdev()
    def inspect_cabal(self, cabal=None):
        try:
            with Common.status_message_process('Inspecting {0}'.format(cabal or 'cabal'), priority=1) as s:
                self.client_back.scan(cabal=(cabal == 'cabal'),
                                      sandboxes=[] if cabal == 'cabal' else [cabal],
                                      on_notify=scan_status(s),
                                      wait=True,
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except:
            Logging.log('loading standard modules info for {0} failed:'.format(cabal or 'cabal'),
                        Logging.LOG_ERROR)
            traceback.print_exception(sys.exc_info()[0], sys.exc_info()[1], sys.exc_info()[2])

    @use_hsdev()
    @use_inspect_modules
    def inspect(self, paths, projects, files):
        if paths or projects or files:
            try:
                with Common.status_message_process('Inspecting', priority=1) as s:
                    self.client_back.scan(paths=paths,
                                          projects=projects,
                                          files=files,
                                          on_notify=scan_status(s),
                                          wait=True,
                                          ghc=Settings.get_setting_async('ghc_opts'),
                                          docs=Settings.get_setting_async('enable_hdocs'))
            except Exception as e:
                Logging.log('Inspection failed: {0}'.format(e), Logging.LOG_ERROR)

    @use_hsdev()
    @use_inspect_modules
    def inspect_path(self, path):
        try:
            with Common.status_message_process('Inspecting path {0}'.format(path), priority=1) as s:
                self.client_back.scan(paths=[path],
                                      on_notify=scan_status(s),
                                      wait=True,
                                      ghc=Settings.get_setting_async('ghc_opts'),
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except Exception as e:
            Logging.log('Inspecting path {0} failed: {1}'.format(path, e), Logging.LOG_ERROR)

    @use_hsdev()
    @use_inspect_modules
    def inspect_project(self, cabal_dir):
        (project_name, cabal_file) = Common.get_cabal_in_dir(cabal_dir)

        try:
            with Common.status_message_process('Inspecting project {0}'.format(project_name), priority=1) as s:
                self.client_back.scan(projects=[cabal_dir],
                                      on_notify=scan_status(s),
                                      wait=True,
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except Exception as e:
            Logging.log('Inspecting project {0} failed: {1}'.format(cabal_dir, e), Logging.LOG_ERROR)

    @use_hsdev()
    @use_inspect_modules
    def inspect_files(self, filenames):
        try:
            with Common.status_message_process('Inspecting files', priority=1) as s:
                self.client_back.scan(files=filenames,
                                      on_notify=scan_status(s),
                                      wait=True,
                                      ghc=Settings.get_setting_async('ghc_opts'),
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except Exception as e:
            Logging.log('Inspecting files failed: {0}'.format(e), Logging.LOG_ERROR)


class HsDevWindowCommand(Common.SublimeHaskellWindowCommand):
    def is_enabled(self):
        return Settings.get_setting_async('enable_hsdev') and \
               agent_connected() and \
               Common.SublimeHaskellWindowCommand.is_enabled(self)

    def is_visible(self):
        return Settings.get_setting_async('enable_hsdev') and Common.SublimeHaskellWindowCommand.is_visible(self)


class HsDevTextCommand(Common.SublimeHaskellTextCommand):
    def is_enabled(self):
        return Settings.get_setting_async('enable_hsdev') and \
               agent_connected() and \
               Common.SublimeHaskellTextCommand.is_enabled(self)

    def is_visible(self):
        return Settings.get_setting_async('enable_hsdev') and Common.SublimeHaskellTextCommand.is_visible(self)


def start_agent():
    global agent
    global client
    global client_back

    if agent is None:
        Logging.log('starting agent', Logging.LOG_TRACE)

        agent = HsDevAgent()
        agent.start()
        client = agent.client
        client_back = agent.client_back
