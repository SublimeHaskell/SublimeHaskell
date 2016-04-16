# -*- coding: UTF-8 -*-

import os
import os.path
import sys
import socket
import sublime
import sublime_plugin
import subprocess
import threading
import json
import time
import re
from functools import reduce

if int(sublime.version()) < 3000:
    import symbols
    from sublime_haskell_common import *
else:
    import SublimeHaskell.symbols as symbols
    from SublimeHaskell.sublime_haskell_common import *

def concat_args(args):
    def cat(x, y):
        (px, ex) = x
        (py, ey) = y
        return (px or py, (ex if px else []) + (ey if py else []))
    return reduce(cat, args, (True, []))[1]

def concat_opts(opts):
    def cat(x, y):
        (px, ex) = x
        (py, ey) = y
        v = (ex if px else {}).copy()
        v.update((ey if py else {}).copy())
        return (px or py, v)
    return reduce(cat, opts, (True, {}))[1]

# {'x': ['1','2'], 'y': None} ⇒ ['--x', '1', '--x', '2', '--y']
def flatten_opts(opts):
    r = []

    def to_opt(x):
        return '--{0}'.format(x)

    for k, v in opts.items():
        if v is None:
            r.append(to_opt(k))
        elif type(v) is list:
            for n in v:
                r.extend([to_opt(k), str(n)])
        else:
            r.extend([to_opt(k), str(v)])

    return r

def hsdev_enabled():
    return get_setting_async('enable_hsdev') == True

def hsdev_enable(enable = True):
    set_setting_async('enable_hsdev', enable)

def hsdev_version():
    try:
        (exit_code, out, err) = call_and_wait(['hsdev', 'version'])
        if exit_code == 0:
            m = re.match('(?P<major>\d+)\.(?P<minor>\d+)\.(?P<revision>\d+)\.(?P<build>\d+)', out)
            if m:
                major = int(m.group('major'))
                minor = int(m.group('minor'))
                revision = int(m.group('revision'))
                build = int(m.group('build'))
                return [major, minor, revision, build]
    except FileNotFoundError:
        pass
    return None

def show_version(ver):
    return '.'.join(map(lambda i: str(i), ver))

def check_version(ver, minimal = [0, 0, 0, 0], maximal = None):
    if ver is None:
        return False
    if ver < minimal:
        return False
    if maximal and ver >= maximal:
        return False
    return True

def if_some(x, lst):
    return lst if x is not None else []

def cabal_path(cabal):
    if not cabal:
        return []
    return ["--cabal"] if cabal == 'cabal' else ["--sandbox={0}".format(cabal)]

def hsinspect(module = None, file = None, cabal = None, ghc_opts = []):
    cmd = ['hsinspect']
    on_result = lambda s: s
    if module:
        cmd.extend([module])
        on_result = parse_module
    elif file:
        cmd.extend([file])
        on_result = parse_module
    elif cabal:
        cmd.extend([cabal])
    else:
        log('hsinspect must specify module, file or cabal', log_debug)
        return None

    for opt in ghc_opts:
        cmd.extend(['-g', opt])

    r = call_and_wait_tool(cmd, 'hsinspect', lambda s: json.loads(s), file, None)
    if r:
        if 'error' in r:
            log('hsinspect returns error: {0}'.format(r['error']), log_error)
        else:
            return on_result(r)
    return None

def print_status(s):
    print(s['status'])

def parse_database(s):
    if not s:
        return None
    if s and 'projects' in s and 'modules' in s:
        return (s['projects'], [parse_module(m) for m in s['modules']])
    return None

def parse_decls(s):
    if s is None:
        return None
    return [parse_module_declaration(decl) for decl in s]

def parse_modules_brief(s):
    if s is None:
        return None
    return [parse_module_id(m) for m in s]

def get_value(dc, ks, defval = None):
    if dc is None:
        return defval
    if type(ks) == list:
        cur = dc
        for k in ks:
            cur = cur.get(k)
            if cur is None:
                return defval
        return cur
    else:
        return dc.get(ks, defval)

# 'global-db', 'user-db' or {'package-db':path}
def parse_package_db(d, defval = None):
    if type(d) == dict:
        pdb = get_value(d, 'package-db')
        return symbols.PackageDb(package_db = pdb) if pdb else defval
    if d == 'global-db':
        return symbols.PackageDb(global_db = True)
    if d == 'user-db':
        return symbols.PackageDb(user_db = True)
    return defval

def parse_position(d):
    if not d:
        return None
    line = get_value(d, 'line')
    column = get_value(d, 'column')
    if line is not None and column is not None:
        return symbols.Position(line, column)
    return None

def parse_location(d):
    loc = symbols.Location(
        get_value(d, 'file'),
        get_value(d, 'project'))
    if not loc.is_null():
        return loc
    loc = symbols.InstalledLocation(
        symbols.parse_package(get_value(d, 'package')),
        parse_package_db(get_value(d, 'db')))
    if not loc.is_null():
        return loc
    loc = symbols.OtherLocation(
        get_value(d, 'source'))
    if not loc.is_null():
        return loc
    return None

def parse_import(d):
    if not d:
        return None
    return symbols.Import(d['name'], d['qualified'], d.get('as'), parse_position(d.get('pos')))

def parse_module_id(d):
    if d is None:
        return None
    return symbols.Module(
        d['name'],
        [], [], {},
        parse_location(d.get('location')))

def parse_declaration(decl):
    try:
        what = decl['decl']['what']
        docs = crlf2lf(decl.get('docs'))
        name = decl['name']
        pos = parse_position(decl.get('pos'))
        imported = []
        if 'imported' in decl and decl['imported']:
            imported = [parse_import(d) for d in decl['imported']]
        defined = None
        if 'defined' in decl and decl['defined']:
            defined = parse_module_id(decl['defined'])

        if what == 'function':
            return symbols.Function(name, decl['decl'].get('type'), docs, imported, defined, pos)
        elif what == 'type':
            return symbols.Type(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args', []), decl['decl']['info'].get('def'), docs, imported, defined, pos)
        elif what == 'newtype':
            return symbols.Newtype(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args', []), decl['decl']['info'].get('def'), docs, imported, defined, pos)
        elif what == 'data':
            return symbols.Data(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args', []), decl['decl']['info'].get('def'), docs, imported, defined, pos)
        elif what == 'class':
            return symbols.Class(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args', []), decl['decl']['info'].get('def'), docs, imported, defined, pos)
        else:
            return None
    except Exception as e:
        log('Error pasring declaration: {0}'.format(e), log_error)
        return None

def parse_declarations(decls):
    if decls is None:
        return None
    return [parse_declaration(d) for d in decls]

def parse_module_declaration(d, parse_module_info = True):
    try:
        m = None
        if 'module-id' in d and parse_module_info:
            m = parse_module_id(d['module-id'])

        loc = parse_location(d['module-id'].get('location'))
        decl = parse_declaration(d['declaration'])

        if not decl:
            return None

        decl.module = m

        return decl
    except:
        return None

def parse_module(d):
    if d is None:
        return None
    return symbols.Module(
        d['name'],
        d.get('exports'),
        [parse_import(i) for i in d['imports']] if 'imports' in d else [],
        dict((decl['name'],parse_declaration(decl)) for decl in d['declarations']) if 'declarations' in d else {},
        parse_location(d.get('location')))

def parse_modules(ds):
    if ds is None:
        return None
    return [parse_module(d) for d in ds]

def parse_cabal_package(d):
    if d is None:
        return None
    return symbols.CabalPackage(
        d['name'],
        d.get('synopsis'),
        d.get('default-version'),
        d.get('installed-versions'),
        d.get('homepage'),
        d.get('license'))

def parse_corrections(d):
    if d is None:
        return None
    return [parse_correction(c) for c in d]

def parse_correction(d):
    return symbols.Correction(
        d['source']['file'],
        d['level'],
        d['note']['message'],
        parse_corrector(d['note']['corrector']))

def parse_corrector(d):
    return symbols.Corrector(
        parse_position(d['region']['from']),
        parse_position(d['region']['to']),
        d['contents'])

def encode_corrections(cs):
    return [encode_correction(c) for c in cs]

def encode_correction(c):
    return {
        'source': {
            'project': None,
            'file': c.file },
        'level': c.level,
        'note': {
            'corrector': encode_corrector(c.corrector),
            'message': c.message },
        'region': {
            'from': encode_position(c.corrector.start.from_zero_based()),
            'to': encode_position(c.corrector.end.from_zero_based()) }
        }

def encode_corrector(c):
    return {
        'region': {
            'from': encode_position(c.start),
            'to': encode_position(c.end) },
        'contents': c.contents }

def encode_position(p):
    return {
        'line': p.line,
        'column': p.column }

def encode_package_db(db):
    if db.user_db:
        return 'user-db'
    if db.global_db:
        return 'global-db'
    if db.package_db:
        return {'package-db': db.package_db}
    return None

def reconnect_function(fn):
    def wrapped(self, *args, **kwargs):
        autoconnect_ = kwargs.pop('autoconnect', False)
        on_reconnect_ = kwargs.pop('on_reconnect', None)
        just_connect_ = kwargs.pop('just_connect', False)
        def run_fn():
            if not just_connect_:
                self.autoconnect = autoconnect_
                self.on_reconnect = on_reconnect_
            return fn(self, *args, **kwargs)
        if not just_connect_:
            self.set_reconnect_function(run_fn)
        return run_fn()
    return wrapped

class begin_connecting(object):
    def __init__(self, agent):
        self.agent = agent

    def __enter__(self):
        self.agent.set_connecting()
        return self

    def __exit__(self, type, value, traceback):
        if type:
            self.agent.set_unconnected()
        else:
            if self.agent.is_connecting():
                self.agent.set_unconnected()

def connect_function(fn):
    def wrapped(self, *args, **kwargs):
        if self.is_unconnected():
            with begin_connecting(self):
                return fn(self, *args, **kwargs)
        else:
            log('hsdev already connected', log_warning)
    return wrapped

def hsdev_command(async = False, timeout = None, is_list = False):
    def wrap_function(fn):
        def wrapped(self, *args, **kwargs):
            wait_flag = kwargs.pop('wait', not async)
            timeout_arg = kwargs.pop('timeout', timeout)
            on_resp = kwargs.pop('on_response', None)
            on_not = kwargs.pop('on_notify', None)
            on_err = kwargs.pop('on_error', None)
            on_res_part = kwargs.pop('on_result_part', None)
            split_res = kwargs.pop('split_result', on_res_part is not None)

            (name_, opts_, on_result_) = fn(self, *args, **kwargs)

            if is_list and split_res:
                result = []
                def on_notify(n):
                    if 'result-part' in n:
                        rp = on_result_([n['result-part']])[0]
                        call_callback(on_res_part, rp)
                        result.append(rp)
                    else:
                        call_callback(on_not, n)
                def on_response(r):
                    on_resp(result)

                opts_.update({'split-result': None}) # FIXME: Is this option still used?
                r = self.call(
                    name_,
                    opts_,
                    on_response = on_response if on_resp else None,
                    on_notify = on_notify,
                    on_error = on_err,
                    wait = wait_flag,
                    timeout = timeout_arg)
                if wait_flag:
                    return result
                return r

            else:
                def on_response(r):
                    on_resp(on_result_(r))
                r = self.call(
                    name_,
                    opts_,
                    on_response = on_response if on_resp else None,
                    on_notify = on_not,
                    on_error = on_err,
                    wait = wait_flag,
                    timeout = timeout_arg)
                if wait_flag:
                    return on_result_(r)
                return r
        return wrapped
    return wrap_function

def command(fn):
    return hsdev_command(async = False, timeout = 1)(fn)

def async_command(fn):
    return hsdev_command(async = True)(fn)

def list_command(fn):
    return hsdev_command(async = False, timeout = 1, is_list = True)(fn)

def async_list_command(fn):
    return hsdev_command(async = True, is_list = True)(fn)

def cmd(name_, opts_ = {}, on_result = lambda r: r):
    return (name_, opts_, on_result)

def call_callback(fn, *args, **kwargs):
    name = kwargs.get('name')
    if name:
        del kwargs['name']
    try:
        if fn is not None:
            fn(*args, **kwargs)
    except Exception as e:
        log("callback '{0}' throws exception: {1}".format(name or '<unnamed>', e))


class HsDevCallbacks(object):
    def __init__(self, id, command, on_response = None, on_notify = None, on_error = None):
        self.id = id
        self.command = command
        self.start_time = time.clock()
        self.on_response = on_response
        self.on_notify = on_notify
        self.on_error = on_error

    def time(self):
        return time.clock() - self.start_time if self.start_time is not None else None

    def log_time(self):
        log('{0}: {1} seconds'.format(self.command, self.time()), log_trace)

    def call_response(self, r):
        self.log_time()
        call_callback(self.on_response, r)

    def call_notify(self, n):
        call_callback(self.on_notify, n)

    def call_error(self, e, ds):
        self.log_time()
        log('{0} returns error: {1}, {2}'.format(self.command, e, ', '.join(['{}: {}'.format(k, v) for k, v in e.items()])), log_error)
        call_callback(self.on_error, e, ds)


class HsDev(object):
    def __init__(self, port = 4567):
        self.port = port
        self.connecting = threading.Event()
        self.connected = threading.Event()
        self.socket = None
        self.listener = None
        self.hsdev_address = None
        self.autoconnect = True
        self.map = LockedObject({})
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
            log('Reconnecting to hsdev...', log_info)
            call_callback(self.on_reconnect, name = 'HsDev.on_reconnect')
            self.connect_fun()
        else:
            log('No reconnect function')

    # Util

    @staticmethod
    def run_server(port = 4567, cache = None, log_file = None, log_config = None):
        cmd = concat_args([
            (True, ["hsdev", "run"]),
            (port, ["--port", str(port)]),
            (cache, ["--cache", cache]),
            (log_file, ["--log", log_file]),
            (log_config, ["--log-config", log_config])])

        log('Starting hsdev server', log_info)
        p = call_and_wait(cmd, wait = False)
        if not p:
            log('Failed creating hsdev process', log_error)
            return None
        while True:
            output = crlf2lf(decode_bytes(p.stdout.readline()))
            m = re.match(r'^.*?hsdev> Server started at port (?P<port>\d+)$', output)
            if m:
                log('hsdev server started at port {0}'.format(m.group('port')))
                p.stdout.close()
                p.stderr.close()
                return p

    @staticmethod
    def start_server(port = 4567, cache = None, log_file = None, log_config = None):
        cmd = concat_args([
            (True, ["hsdev", "start"]),
            (port, ["--port", str(port)]),
            (cache, ["--cache", cache]),
            (log_file, ["--log", log_file]),
            (log_config, ["--log-config", log_config])])

        def parse_response(s):
            try:
                return {} if s.isspace() else json.loads(s)
            except Exception as e:
                return {'error': 'Invalid response', 'details': s}

        log('Starting hsdev server', log_info)

        ret = call_and_wait_tool(cmd, 'hsdev', '', None, None, None, check_enabled = False)
        if ret is not None:
            return ret
        return None

    # Static creators

    @staticmethod
    def client(port = 4567, cache = None, autoconnect = False):
        start_server(port = port, cache = cache)
        h = HsDev(port = port)
        h.connect(autoconnect = autoconnect)
        return h

    @staticmethod
    def client_async(port = 4567, cache = None, autoconnect = False):
        start_server(port = port, cache = cache)
        h = HsDev(port = port)
        h.connect_async(autoconnect = autoconnect)
        return h

    # Socket functions

    @connect_function
    @reconnect_function
    def connect(self, tries = 10, delay = 1.0):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        for n in range(0, tries):
            try:
                log('connecting to hsdev server ({0})...'.format(n), log_info)
                self.socket.connect(('127.0.0.1', self.port))
                self.hsdev_socket = self.socket
                self.hsdev_address = '127.0.0.1'
                self.set_connected()
                self.listener = threading.Thread(target = self.listen)
                self.listener.start()
                log('connected to hsdev server', log_info)
                call_callback(self.on_connected, name = 'HsDev.on_connected')
                return True
            except Exception as e:
                log('failed to connect to hsdev server ({0})'.format(n), log_warning)
                time.sleep(delay)

        return False

    @reconnect_function
    def connect_async(self, tries = 10, delay = 1.0):
        thread = threading.Thread(
            target = self.connect,
            kwargs = { 'tries' : tries, 'delay': delay, 'just_connect' : True })
        thread.start()
  
    def wait(self, timeout = None):
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
            log('HsDev.set_connected called while not in connecting state', log_debug)

    def on_receive(self, id, command, on_response = None, on_notify = None, on_error = None):
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
        log('{0}: connection to hsdev lost: {1}'.format(fn, e), log_error)
        call_callback(self.on_disconnected, name = 'HsDev.on_disconnected')

        # send error to callbacks
        with self.map as m:
            for on_msg in m.values():
                on_msg.on_error('connection lost')
            m.clear()

        self.id = 1
        self.part = ''

        if self.autoconnect:
            self.reconnect()

    def call(self, command, opts = {}, on_response = None, on_notify = None, on_error = None, wait = False, timeout = None, id = None):
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
                call_callback(on_response, r)
                if wait_receive:
                    wait_receive.set()

            def on_error_(e):
                call_callback(on_error, e)
                if wait_receive:
                    wait_receive.set()

            if wait or on_response or on_notify or on_error:
                if id is None:
                    id = str(self.id)
                    self.id = self.id + 1
                self.on_receive(id, args_cmd, on_response_, on_notify, on_error_)

            opts.update({'no-file': True})
            opts.update({'id': id, 'command': command})
            msg = json.dumps(opts, separators = (',', ':'))

            # Seems, that first sendall doesn't throw error on closed socket
            # So we just call it twice
            # It's hackish, but I haven't found easy solution
            self.hsdev_socket.sendall(msg.encode('utf-8'))
            self.hsdev_socket.sendall('\n'.encode('utf-8'))
            log(call_cmd, log_trace)

            if wait:
                wait_receive.wait(timeout)
                return x.get('result')

            return True
        except Exception as e:
            log('{0} fails with exception: {1}'.format(call_cmd, e), log_error)
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
        while not '\n' in self.part:
            self.part = self.part + self.socket.recv(65536).decode('utf-8')
        (r, _, post) = self.part.partition('\n')
        self.part = post
        return r

    # Commands

    @command
    def link(self, hold = False, **kwargs):
        return cmd('link', {
            'hold': hold })

    @command
    def ping(self):
        return cmd('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'))

    @async_command
    def scan(self, cabal = False, sandboxes = [], projects = [], files = [], paths = [], ghc = [], contents = {}, docs = False, infer = False):
        return cmd('scan', {
            'projects': projects,
            'cabal': cabal,
            'sandboxes': sandboxes,
            'files': files,
            'paths': paths,
            'contents': [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'ghc-opts': ghc,
            'docs': docs,
            'infer': infer })

    @async_command
    def docs(self, projects = [], files = [], modules = []):
        return cmd('docs', {
            'projects': projects,
            'files': files,
            'modules': modules })

    @async_command
    def infer(self, projects = [], files = [], modules = []):
        return cmd('infer', {
            'projects': projects,
            'files': files,
            'modules': modules })

    @async_list_command
    def remove(self, cabal = False, sandboxes = [], projects = [], files = [], packages = []):
        return cmd('remove', {
            'projects': projects,
            'cabal': cabal,
            'sandboxes': sandboxes,
            'files': files,
            'packages': packages })

    @command
    def remove_all(self):
        return cmd('remove-all', {})

    @list_command
    def list_modules(self, project = None, file = None, module = None, deps = None, sandbox = None, cabal = False, db = None, package = None, source = False, standalone = False):
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
            fs.append({'cabal':{'sandbox': sandbox}})
        if cabal:
            fs.append({'cabal':'cabal'})
        if db:
            fs.append({'db': encode_package_db(db)})
        if package:
            fs.append({'package': package})
        if source:
            fs.append('sourced')
        if standalone:
            fs.append('standalone')

        return cmd('modules', {'filters': fs}, parse_modules_brief)

    @list_command
    def list_packages(self):
        return cmd('packages', {})

    @list_command
    def list_projects(self):
        return cmd('projects', {})

    @list_command
    def symbol(self, input = "", search_type = 'prefix', project = None, file = None, module = None, deps = None, sandbox = None, cabal = False, db = None, package = None, source = False, standalone = False, locals = False):
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
            fs.append({'cabal':{'sandbox': sandbox}})
        if cabal:
            fs.append({'cabal':'cabal'})
        if db:
            fs.append({'db': encode_package_db(db)})
        if package:
            fs.append({'package': package})
        if source:
            fs.append('sourced')
        if standalone:
            fs.append('standalone')

        return cmd('symbol', {'query': q, 'filters': fs, 'locals': locals}, parse_decls)

    @command
    def module(self, input = "", search_type = 'prefix', project = None, file = None, module = None, deps = None, sandbox = None, cabal = False, db = None, package = None, source = False, standalone = False):
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
            fs.append({'cabal':{'sandbox': sandbox}})
        if cabal:
            fs.append({'cabal':'cabal'})
        if db:
            fs.append({'db': encode_package_db(db)})
        if package:
            fs.append({'package': package})
        if source:
            fs.append('sourced')
        if standalone:
            fs.append('standalone')

        return cmd('module', {'query': q, 'filters': fs}, parse_modules)

    @command
    def resolve(self, file, exports = False):
        return cmd('resolve', {'file': file, 'exports': exports}, parse_module)

    @command
    def project(self, project = None, path = None):
        return cmd('project', {'name': project} if project else {'path': path})

    @command
    def sandbox(self, path):
        return cmd('sandbox', {'path': path})

    @list_command
    def lookup(self, name, file):
        return cmd('lookup', {'name': name, 'file': file}, parse_decls)

    @list_command
    def whois(self, name, file):
        return cmd('whois', {'name': name, 'file': file}, parse_declarations)

    @list_command
    def scope_modules(self, file, input = '', search_type = 'prefix'):
        return cmd('scope modules', {'query': {'input': input, 'type': search_type}, 'file': file}, parse_modules_brief)

    @list_command
    def scope(self, file, input = '', search_type = 'prefix', global_scope = False):
        return cmd('scope', {'query': {'input': input, 'type': search_type}, 'global': global_scope, 'file': file}, parse_declarations)

    @list_command
    def complete(self, input, file, wide = False):
        return cmd('complete', {'prefix': input, 'wide': wide, 'file': file}, parse_declarations)

    @list_command
    def hayoo(self, query, page = None, pages = None):
        return cmd('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1}, parse_decls)

    @list_command
    def cabal_list(self, packages):
        cmd('cabal list', {'packages': packages}, lambda r: [parse_cabal_package(s) for s in r] if r else None)

    @list_command
    def lint(self, files = [], contents = {}, hlint = []):
        return cmd('lint', {
            'files': files,
            'contents': [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'hlint-opts': hlint})

    @list_command
    def check(self, files = [], contents = {}, ghc = []):
        return cmd('check', {
            'files': files,
            'contents': [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'ghc-opts': ghc})

    @list_command
    def check_lint(self, files = [], contents = {}, ghc = [], hlint = []):
        return cmd('check-lint', {
            'files': files,
            'contents': [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'ghc-opts': ghc,
            'hlint-opts': hlint})

    @list_command
    def types(self, files = [], contents = {}, ghc = []):
        return cmd('types', {
            'files': files,
            'contents': [{'file': f, 'contents': cts} for f, cts in contents.items()],
            'ghc-opts': ghc})

    @command
    def ghcmod_lang(self):
        return cmd('ghc-mod lang')

    @command
    def ghcmod_flags(self):
        return cmd('ghc-mod flags')

    @list_command
    def ghcmod_type(self, file, line, column = 1, ghc = []):
        return cmd('ghc-mod type', {
            'position': {'line': int(line),'column': int(column)},
            'file': file,
            'ghc-opts': ghc })

    @list_command
    def ghcmod_check(self, files, ghc = []):
        return cmd('ghc-mod check', {'files': files, 'ghc-opts': ghc})

    @list_command
    def ghcmod_lint(self, files, hlint = []):
        return cmd('ghc-mod lint', {'files': files, 'hlint-opts': hlint})

    @list_command
    def ghcmod_check_lint(self, files, ghc = [], hlint = []):
        return cmd('ghc-mod check-lint', {'files': files, 'ghc-opts': ghc, 'hlint-opts': hlint})

    @list_command
    def autofix_show(self, messages):
        return cmd('autofix show', {'messages': messages}, parse_corrections)

    @list_command
    def autofix_fix(self, messages, rest = [], pure = False):
        return cmd('autofix fix', {'messages': messages, 'rest': rest, 'pure': pure}, parse_corrections)

    @list_command
    def ghc_eval(self, exprs):
        return cmd('ghc eval', {'exprs': exprs})

    @command
    def exit(self):
        return cmd('exit', {})

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
    def wait_error(e):
        log('hsdev call fails with: {0}'.format(e))
        if on_err:
            on_err(e)
        wait_receive.set()

    tm = kwargs.pop('timeout', 0.1)

    kwargs['on_response'] = wait_response
    kwargs['on_error'] = wait_error

    fn(*args, **kwargs)

    wait_receive.wait(tm)
    return x['result']

class HsDevProcess(threading.Thread):
    def __init__(self, port = 4567, cache = None, log_file = None, log_config = None):
        super(HsDevProcess, self).__init__()
        self.process = None
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
                self.process = HsDev.run_server(port = self.port, cache = self.cache, log_file = self.log_file, log_config = self.log_config)
                if not self.process:
                    log('failed to create hsdev process', log_error)
                    self.stop_event.set()
                else:
                    call_callback(self.on_start, name = 'HsDevProcess.on_start')
                self.process.wait()
                call_callback(self.on_exit, name = 'HsDevProcess.on_exit')
            self.stop_event.clear()

    def active(self):
        return self.process.poll() is None

    def inactive(self):
        return self.process.poll() is not None

    def create(self):
        self.create_event.set()

    def stop(self):
        self.stop_event.set()
