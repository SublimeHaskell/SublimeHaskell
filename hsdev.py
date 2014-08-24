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

def use_hsdev(fn):
    def wrapped(*args, **kwargs):
        if hsdev_enabled():
            return fn(*args, **kwargs)
    return wrapped

def call_hsdev_and_wait(arg_list, filename = None, cabal = None, callback = None, **popen_kwargs):
    cmd = ['hsdev'] + arg_list

    result = None

    def on_line(l):
        if l:
            if 'status' in l:
                callback(l)
            else:
                result = l

    def parse_response(s):
        try:
            return {} if s.isspace() else json.loads(s)
        except Exception as e:
            return {'error' : 'invalid response', 'details' : s}

    log(' '.join(cmd), log_trace)
    ret = call_and_wait_tool(cmd, 'hsdev', parse_response, filename, on_line if callback else None, check_enabled = False, **popen_kwargs)
    if ret is not None:
        result = ret

    return result

def hsdev(arg_list, port = None, on_response = None):
    r = call_hsdev_and_wait(if_some(port, ['--port', str(port)]) + arg_list, callback = on_response)
    if r is None:
        return None
    if r and 'error' in r:
        log('hsdev returns error: {0} with details: {1}'.format(r['error'], r['details']), log_error)
        return None
    return r

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
        cmd.extend(['module', module])
        on_result = parse_module
    elif file:
        cmd.extend(['file', file])
        on_result = parse_module
    elif cabal:
        cmd.extend(['cabal', cabal])
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

class StatusToMessage(object):
    def __init__(messager):
        self.messager = messager

    def on_status(self, s):
        (task_name, info) = s['task'].values()[0]
        cur = s['progress']['current']
        total = s['progress']['total']
        s.change_message('{0} {1}: {2}'.format(task_name, info, s['status']))
        s.percentage_message(cur, total)

def start(port = None, cache = None, log = None):
    return hsdev(['server', 'start'] + if_some(cache, ['--cache', cache]) + if_some(log, ['--log', log]), port = port) is not None

def link(port = None, parent = None):
    return hsdev(['link'] + if_some(parent, ['--parent', parent]), port = port) is not None

def stop(port = None):
    return hsdev(['server', 'stop'], port = port) is not None

def ping(port = None):
    return hsdev(['ping'], port = port) == "pong"

def scan(cabal = None, projects = [], files = [], paths = [], modules = [], wait = False, on_status=None, port = None):
    opts = ['scan']
    if modules:
        opts.extend(['module'] + modules)
        if cabal:
            opts.extend(['--sandbox', cabal])
    elif cabal:
        opts.extend(['cabal'] + cabal_path(cabal))
    else:
        args = [['--project', p] for p in projects] + [['-f', f] for f in files] + [['-p', p] for p in paths]
        opts.extend(list(reduce(lambda x, y: x + y, args)))

    if wait or on_status:
        opts.extend(['-w', '-s'])

    opts.extend(get_ghc_opts_args(cabal = cabal))

    def onResponse(s):
        if on_status:
            on_status(s)

    return hsdev(opts, port = port, on_response = onResponse if wait else None)

def rescan(projects = [], files = [], paths = [], wait = False, on_status = None, port = None):
    opts = ['rescan']
    args = [['--project', p] for p in projects] + [['-f', f] for f in files] + [['-p', p] for p in paths]

    if not args:
        log('hsdev.rescan: must specify at least one param', log_debug)
        return None

    opts.extend(list(reduce(lambda x, y: x + y, args)))

    if wait or on_status:
        opts.extend(['-w', '-s'])

    opts.extend(get_ghc_opts_args(filename = file))

    def onResponse(s):
        if on_status:
            on_status(s)

    return hsdev(opts, port = port, on_response = onResponse if wait else None)

def remove(cabal = None, project = None, file = None, module = None, port = None):
    return hsdev(
        ['remove'] +
        cabal_path(cabal) +
        if_some(project, ['--project', project]) +
        if_some(file, ['-f', file]) +
        if_some(module, ['-m', module]), port = port)

def remove_all(port = None):
    return hsdev(['remove', '-a'], port = port)

def list_modules(cabal = None, project = None, package = None, source = False, standalone = False, port = None):
    return parse_modules(
        hsdev(
            ['modules'] +
            cabal_path(cabal) +
            if_some(project, ['--project', project]) +
            if_some(package, ['--package', package]) +
            (['--src'] if source else []) +
            (['--stand'] if standalone else []), port = port))

def list_packages(port = None):
    return hsdev(['packages'], port = port)

def list_projects(port = None):
    return hsdev(['projects'], port = port)

def symbol(name = None, project = None, file = None, module = None, locals = False, package = None, cabal = None, source = False, standalone = False, prefix = None, find = None, port = None):
    return parse_decls(
        hsdev(
            (['symbol', name] if name else ['symbol']) +
            if_some(project, ['--project', project]) +
            if_some(file, ['-f', file]) +
            if_some(module, ['-m', module]) +
            (['--locals'] if locals else []) +
            if_some(package, ['--package', package]) +
            cabal_path(cabal) +
            (['--src'] if source else []) +
            (['--stand'] if standalone else []) +
            if_some(prefix, ['--prefix', prefix]) +
            if_some(find, ['--find', find]), port = port))

def module(name = None, locals = False, package = None, project = None, file = None, cabal = None, source = False, port = None):
    return parse_module(
        hsdev(
            ['module'] +
            (['--locals'] if locals else []) +
            if_some(name, ['-m', name]) +
            if_some(package, ['--package', package]) +
            if_some(project, ['--project', project]) +
            cabal_path(cabal) +
            if_some(file, ['-f', file]) +
            (['--src'] if source else []), port = port))

def project(project, port = None):
    return hsdev(['project', '--project', project], port = port)

def lookup(name, file, cabal = None, port = None):
    return parse_decls(
        hsdev(
            ['lookup', name, '-f', file] + cabal_path(cabal), port = port))

def whois(name, file, cabal = None, port = None):
    return parse_decls(
        hsdev(
            ['whois', name, '-f', file] + cabal_path(cabal), port = port))

def scope_modules(file, cabal = None, port = None):
    return parse_modules(
        hsdev(
            ['scope', 'modules', '-f', file] + cabal_path(cabal), port = port))

def scope(file, cabal = None, global_scope = False, prefix = None, find = None, port = None):
    return parse_decls(
        hsdev(
            ['scope', '-f', file] +
            cabal_path(cabal) +
            (['--global'] if global_scope else []) +
            if_some(prefix, ['--prefix', prefix]) +
            if_some(find, ['--find', find]), port = port))

def complete(input, file, cabal = None, port = None):
    return parse_decls(
        hsdev(
            ['complete', input, '-f', file] + cabal_path(cabal), port = port))

def hayoo(query, port = None):
    return parse_decls(hsdev(['hayoo', query], port = port))

def cabal_list(query = None, port = None):
    r = hsdev(['cabal', 'list'] + ([query] if query else []), port = port)
    if r is None:
        return None
    return [parse_cabal_package(s) for s in r]

def ghcmod_type(file, line, column = 1, cabal = None, port = None):
    return hsdev(['ghc-mod', 'type', '--file', file, str(line), str(column)] + cabal_path(cabal), port = port)

def dump(cabal = None, projects = [], files = [], path = None, file = None, port = None):
    opts = ['dump']
    if cabal:
        opts.extend(['cabal'] + cabal_path(cabal))
    elif projects:
        opts.extend(['project'] + projects)
    elif files:
        opts.extend(['standalone'] + files)
    
    if path:
        opts.extend(['-p', path])
    if file:
        opts.extend(['-f', file])

    r = hsdev(opts, port = port)
    if r:
        return parse_database(r)
    else:
        return r

def load(path = None, file = None, data = None, port = None):
    return hsdev(
        ['load'] +
        if_some(path, ['-p', path]) +
        if_some(file, ['-f', file]) +
        if_some(data, ['--data', data]), port = port)

def exit(port = None):
    return hsdev(['exit'], port = port)

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

def parse_modules(s):
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

def parse_location(d, p = None):
    loc = symbols.Location(
        get_value(d, 'file'),
        get_value(p, 'line', 0),
        get_value(p, 'column', 0),
        get_value(d, 'project'))
    if not loc.is_null():
        return loc
    loc = symbols.InstalledLocation(
        symbols.parse_package(get_value(d, 'package')),
        get_value(d, 'cabal'))
    if not loc.is_null():
        return loc
    loc = symbols.OtherLocation(
        get_value(d, 'source'))
    if not loc.is_null():
        return loc
    return None

def parse_cabal(d):
    c = get_value(d, 'cabal')
    if c == '<cabal>':
        return 'cabal'
    else:
        return c

def parse_import(d):
    if not d:
        return None
    return symbols.Import(d['name'], d['qualified'], d.get('as'), parse_location(None, d.get('pos')))

def parse_module_id(d):
    if d is None:
        return None
    return symbols.Module(
        d['name'],
        [], [], {},
        parse_location(d.get('location')),
        parse_cabal(d.get('location')))

def parse_declaration(decl):
    try:
        what = decl['decl']['what']
        loc = parse_location(None, decl.get('pos'))
        docs = crlf2lf(decl.get('docs'))
        name = decl['name']

        if what == 'function':
            return symbols.Function(name, decl['decl'].get('type'), docs, loc)
        elif what == 'type':
            return symbols.Type(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args'), decl['decl']['info'].get('def'), docs, loc)
        elif what == 'newtype':
            return symbols.Newtype(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args'), decl['decl']['info'].get('def'), docs, loc)
        elif what == 'data':
            return symbols.Data(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args'), decl['decl']['info'].get('def'), docs, loc)
        elif what == 'class':
            return symbols.Class(name, decl['decl']['info'].get('ctx'), decl['decl']['info'].get('args'), decl['decl']['info'].get('def'), docs, loc)
        else:
            return None
    except Exception as e:
        log('Error pasring declaration: {0}'.format(e), log_error)
        return None

def parse_module_declaration(d, parse_module_info = True):
    try:
        m = None
        if 'module-id' in d and parse_module_info:
            m = parse_module_id(d['module-id'])

        loc = parse_location(d['module-id'].get('location'))
        decl = parse_declaration(d['declaration'])

        if not decl:
            return None

        decl.update_location(loc)

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
        parse_location(d.get('location')),
        parse_cabal(d.get('location')))

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

class HsDevHolder(object):
    def __init__(self, port = 4567, cache = None):
        super(HsDevHolder, self).__init__()
        self.port = port
        self.cache = cache
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.started_event = threading.Event()

    def is_running(self):
        return ping(self.port)

    def run_hsdev(self, tries = 10):
        self.start_hsdev()
        return self.link_hsdev(tries = tries)

    def start_hsdev(self):
        start(port = self.port, cache = self.cache)

    def stop_hsdev(self):
        if self.is_running():
            exit(port = self.port)

    def link_hsdev(self, tries = 10):
        for n in range(0, tries):
            try:
                log('connecting to hsdev server...', log_info)
                self.socket.connect(('127.0.0.1', self.port))
                log('connected to hsdev server', log_info)
                self.socket.sendall(b'{"command":["link"],"opts":{"hold":null}}\n')
                self.started_event.set()
                log('hsdev server started', log_info)
                return True
            except:
                log('failed to connect to hsdev server, wait for a while', log_warning)
                time.sleep(0.1)
        return False

    # Wait until linked
    def wait_hsdev(self, timeout = 60):
        return self.started_event.wait(timeout)

    # Call hsdev function
    def call(self, fn, *args, **kwargs):
        kwargs['port'] = self.port
        return fn(*args, **kwargs)

def reconnect_function(fn):
    def wrapped(self, *args, **kwargs):
        def run_fn():
            self.autoconnect = kwargs.pop('autoconnect', False)
            self.on_reconnect = kwargs.pop('on_reconnect', None)
            return fn(self, *args, **kwargs)
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
    return wrapped

def command(fn):
    def wrapped(self, *args, **kwargs):
        on_resp = kwargs.pop('on_response', None)
        on_not = kwargs.pop('on_notify', None)
        on_err = kwargs.pop('on_error', None)
        (name_, args_, opts_, on_result_) = fn(self, *args, **kwargs)
        def on_response(r):
            if on_result_:
                on_resp(on_result_(r))
            else:
                on_resp(r)
        return self.call(
            name_,
            args_,
            opts_,
            on_response if on_resp else None,
            on_not,
            on_err)
    return wrapped

def cmd(name_, args_, opts_, on_result = None):
    return (name_, args_, opts_, on_result)

def call_callback(name, fn, *args, **kwargs):
    try:
        fn(*args, **kwargs)
    except Exception as e:
        log("callback '{0}' throws exception: {1}".format(name, e))



class HsDev(object):
    def __init__(self, port = 4567):
        self.port = port
        self.connecting = threading.Event()
        self.connected = threading.Event()
        self.socket = None
        self.listener = None
        self.hsdev_address = None
        self.autoconnect = True
        self.map = {}
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
            call_callback('HsDev.on_reconnect', self.on_reconnect)
            self.connect_fun()

    # Util

    def start_server(port = 4567, cache = None):
        cmd = concat_args([
            (True, ["hsdev", "start"]),
            (port, ["--port", str(port)]),
            (cache, ["--cache", cache])])

        def parse_response(s):
            try:
                return {} if s.isspace() else json.loads(s)
            except Exception as e:
                return {'error': 'Invalid response', 'details': s}

        log('Starting hsdev server command: {0}'.format(cmd), log_trace)
        log('Starting hsdev server', log_info)

        ret = call_and_wait_tool(cmd, 'hsdev', parse_response, None, None, check_enabled = False)
        if ret is not None:
            return ret
        return None

    # Static creators

    def client(port = 4567, cache = None, autoconnect = False):
        start_server(port = port, cache = cache)
        h = HsDev(port = port)
        h.connect(autoconnect = autoconnect)
        return h

    def client_async(port = 4567, cache = None, autoconnect = False):
        start_server(port = port, cache = cache)
        h = HsDev(port = port)
        h.connect_async(autoconnect = autoconnect)
        return h

    # Socket functions

    @connect_function
    @reconnect_function
    def connect(self, tries = 10):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        for n in range(0, tries):
            try:
                log('connecting to hsdev server...', log_info)
                self.socket.connect(('127.0.0.1', self.port))
                self.hsdev_socket = self.socket
                self.hsdev_address = '127.0.0.1'
                self.listener = threading.Thread(target = self.listen)
                self.listener.start()
                self.set_connected()
                log('connected to hsdev server', log_info)
                call_callback('HsDev.on_connected', self.on_connected)
                return True
            except Exception as e:
                log('failed to connect to hsdev server', log_warning)
                time.sleep(0.1)

        return False

    @reconnect_function
    def connect_async(self, tries = 10):
        thread = threading.Thread(
            target = self.connect,
            kwargs = { 'tries' : tries })
        thread.start()
  
    def wait(self, timeout = None):
        return self.connected.wait(timeout)

    def close(self):
        if self.is_unconnected():
            return
        if self.hsdev_socket:
            self.hsdev_socket.close()
            self.hsdev_socket = None
        self.socket.close()
        self.connected.clear()

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

    def on_receive(self, id, on_response = None, on_notify = None, on_error = None):
        self.map[id] = (on_response, on_notify, on_error)

    def call(self, command, args = [], opts = {}, on_response = None, on_notify = None, on_error = None, id = None):
        # log
        call_cmd = 'hsdev {0}'.format(' '.join([command] + args + flatten_opts(opts)))

        if not self.is_connected():
            log('Not connected to hsdev', log_error)
            if self.autoconnect:
                self.reconnect()
            if not self.is_connected():
                return False

        try:
            opts.update({'no-file': None})
            msg = json.dumps({
                'id': id,
                'command': command,
                'args': args,
                'opts': opts })

            if on_response or on_notify or on_error:
                self.on_receive(id, on_response, on_notify, on_error)

            self.hsdev_socket.sendall('{0}\n'.format(msg).encode())
            log(call_cmd, log_trace)
            return True
        except Exception as e:
            log('{0} fails with exception: {1}'.format(call_cmd, e), log_error)
            log('Connection to hsdev lost', log_error)
            call_callback('HsDev.on_disconnected', self.on_disconnected)
            self.close()
            if self.autoconnect:
                self.reconnect()
            return False

    def listen(self):
        while True:
            resp = json.loads(self.get_response())
            if 'id' in resp:
                if resp['id'] in self.map:
                    (on_resp, on_not, on_err) = self.map[resp['id']]
                    if 'notify' in resp:
                        if on_not:
                            on_not(resp['notify'])
                    if 'error' in resp:
                        log('hsdev returns error: {0}, details: {1}'.format(resp['error'], resp.get('details')), log_error)
                        if on_err:
                            on_err(resp['error'])
                    if 'result' in resp:
                        if on_resp:
                            on_resp(resp['result'])

    def get_response(self):
        while not '\n' in self.part:
            self.part = self.part + self.socket.recv(65536).decode()
        (r, _, post) = self.part.partition('\n')
        self.part = post
        return r

    # Commands

    @command
    def link(self, hold = False, **kwargs):
        return cmd('link', [], concat_opts([(hold, {'hold': None})]))

    @command
    def ping(self):
        return cmd('ping', [], {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'))

    @command
    def scan(self, cabal = None, sandboxes = [], projects = [], files = [], paths = []):
        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (projects, {'project': projects}),
            (files, {'file': files}),
            (paths, {'path': paths})])

        return cmd('scan', [], opts)

    @command
    def rescan(self, cabal = None, sandboxes = [], projects = [], files = [], paths = []):
        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (projects, {'project': projects}),
            (files, {'file': files}),
            (paths, {'path': paths})])

        return cmd('rescan', [], opts)

    @command
    def remove(self, cabal = False, sandboxes = [], projects = [], files = [], modules = []):
        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (projects, {'project': projects}),
            (files, {'file': files}),
            (modules, {'module': modules})])

        return cmd('remove', [], opts)

    @command
    def remove_all(self):
        return cmd('remove', [], {'all': None})

    @command
    def list_modules(self, cabal = False, sandboxes = None, projects = None, packages = None, source = False, standalone = False):
        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (projects, {'project': projects}),
            (packages, {'package': packages}),
            (source, {'src': None}),
            (standalone, {'stand': None})])

        return cmd('modules', [], opts, parse_modules)

    @command
    def list_packages(self):
        return cmd('packages', [], {})

    @command
    def list_projects(self):
        return cmd('projects', [], {})

    @command
    def symbol(self, name = None, project = None, file = None, module = None, locals = False, package = None, cabal = False, sandbox = None, source = False, standalone = False, prefix = None, find = None):
        opts = concat_opts([
            (project, {'project': project}),
            (file, {'file': file}),
            (module, {'module': module}),
            (locals, {'locals': None}),
            (package, {'package': package}),
            (cabal, {'cabal': None}),
            (sandbox, {'sandbox': sandbox}),
            (source, {'src': None}),
            (standalone, {'stand': None}),
            (prefix, {'prefix': prefix})])

        return cmd('symbol', [name] if name else [], opts, parse_decls)

    @command
    def module(self, name = None, project = None, file = None, locals = False, package = None, cabal = False, sandbox = None, source = False):
        opts = concat_opts([
            (name, {'module': name}),
            (project, {'project': project}),
            (file, {'file': file}),
            (locals, {'locals': None}),
            (package, {'package': package}),
            (cabal, {'cabal': None}),
            (sandbox, {'sandbox': sandbox}),
            (source, {'src': None})])

        return cmd('module', [], opts, parse_module)

    @command
    def project(self, project):
        return cmd('project', [], {'project': project})

    @command
    def lookup(self, name, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return cmd('lookup', [name], opts, parse_decls)

    @command
    def whois(self, name, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return cmd('whois', [name], opts, parse_decls)

    @command
    def scope_modules(self, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return cmd('scope modules', [], opts, parse_modules)

    @command
    def scope(self, file, sandbox = None, global_scope = False, prefix = None, find = None):
        opts = concat_opts([
            (True, {'file': file}),
            (sandbox, {'sandbox': sandbox}),
            (global_scope, {'global': None}),
            (prefix, {'prefix': prefix}),
            (find, {'find': find})])

        return cmd('scope', [], opts, parse_decls)

    @command
    def complete(self, input, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return cmd('complete', [input], opts, parse_decls)

    @command
    def hayoo(self, query):
        return cmd('hayoo', [query], {}, parse_decls)

    @command
    def cabal_list(self, query = None):
        return cmd()
        cmd('cabal list', [query] if query else [], {}, lambda r: [parse_cabal_package(s) for s in r] if r else None)

    @command
    def ghcmod_type(self, file, line, column = 1, sandbox = None):
        opts = concat_opts([
            (True, {'file': file}),
            (sandbox, {'sandbox': sandbox})])

        return cmd('ghc-mod type', [str(line), str(column)], opts)

    @command
    def exit(self):
        return cmd('exit', [], {})

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
