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
            ['list', 'modules'] +
            cabal_path(cabal) +
            if_some(project, ['--project', project]) +
            if_some(package, ['--package', package]) +
            (['--src'] if source else []) +
            (['--stand'] if standalone else []), port = port))

def list_packages(port = None):
    return hsdev(['list', 'packages'], port = port)

def list_projects(port = None):
    return hsdev(['list', 'projects'], port = port)

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
        docs = decl.get('docs')
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

class HsDev(object):
    def __init__(self, port = 4567):
        self.port = port
        self.connected = threading.Event()
        self.socket = None
        self.hsdev_socket = None
        self.hsdev_address = None

    def __del__(self):
        self.close()

    # Util

    def start_server(as_client = False, port = 4567, cache = None):
        cmd = concat_args([
            (True, ["hsdev", "server", "start"]),
            (as_client, ["--as-client"]),
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

    def server(port = 4567, cache = None):
        h = HsDev(port = port)
        h.accept()
        start_server(as_client = True, port = port, cache = cache)
        return h

    def server_async(port = 4567, cache = None):
        h = HsDev(port = port)
        h.accept_async()
        start_server(as_client = True, port = port, cache = cache)
        return h

    def client(port = 4567, cache = None):
        start_server(as_client = False, port = port, cache = cache)
        h = HsDev(port = port)
        h.connect()
        return h

    def client_async(port = 4567, cache = None):
        start_server(as_client = False, port = port, cache = cache)
        h = HsDev(port = port)
        h.connect_async()
        return h

    # Socket functions

    def accept(self):
        if self.connected.is_set():
            return
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.socket.bind(('127.0.0.1', self.port))
        self.socket.listen(1)
        (s, addr) = self.socket.accept()
        self.hsdev_socket = s
        self.hsdev_address = addr
        self.connected.set()

    def accept_async(self):
        thread = threading.Thread(target = self.accept)
        thread.start()

    def connect(self, tries = 10):
        if self.connected.is_set():
            return
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        for n in range(0, tries):
            try:
                log('connecting to hsdev server...', log_info)
                self.socket.connect(('127.0.0.1', self.port))
                self.hsdev_socket = self.socket
                self.hsdev_address = '127.0.0.1'
                self.connected.set()
                log('connected to hsdev server', log_info)
                return True
            except Exception as e:
                log('failed to connect to hsdev server', log_warning)
                time.sleep(0.1)

        return False

    def connect_async(self, tries = 10):
        thread = threading.Thread(target = self.connect)
        thread.start()
  
    def wait(self, timeout = None):
        return self.connected.wait(timeout)

    def close(self):
        if not self.is_connected():
            return
        if self.hsdev_socket:
            self.hsdev_socket.close()
            self.hsdev_socket = None
        self.socket.close()
        self.connected.clear()

    def is_connected(self):
        return self.connected.is_set()

    def call(self, command, args = [], opts = {}, on_status = None):
        if not self.is_connected():
            log('HsDev.call: not connected', log_error)
            return None

        try:
            # log
            call_cmd = 'hsdev {0}'.format(' '.join(command + args + flatten_opts(opts)))

            opts.update({'no-file': None})
            msg = json.dumps({
                'command': command,
                'args': args,
                'opts': opts })

            self.hsdev_socket.sendall('{0}\n'.format(msg).encode())
            r = self.receive_response(on_status)
            if 'error' in r:
                if 'details' in r:
                    log('{0} returns error: {1}, details: {2}'.format(call_cmd, r['error'], r['details']), log_error)
                else:
                    log('{0} returns error: {1}'.format(call_cmd, r['error']), log_error)
                return None
            log(call_cmd, log_trace)
            return r
        except Exception as e:
            log('{0} fails with exception: {1}'.format(call_cmd, e), log_error)
            return None

    def receive_response(self, on_status = None):
        resp = json.loads(self.receive_response_raw())
        if 'status' in resp:
            if on_status:
                on_status(resp)
            return self.receive_response(on_status)
        else:
            return resp

    def receive_response_raw(self):
        part = ''
        while not part.endswith('\n'):
            part = part + self.hsdev_socket.recv(65536).decode()
        return part.rstrip('\n')

    # Commands

    def link(self, hold = False):
        return self.call(['link'], [], concat_opts([(hold, {'hold': None})]))

    def ping(self):
        r = self.call(['ping'], [], {})
        return r and ('message' in r) and (r['message'] == 'pong')

    def scan_cabal(self, cabal = None, sandboxes = [], wait = False, on_status = None):
        if cabal is None:
            cabal = not sandboxes # default: --cabal enabled if not sandboxes specified and disabled otherwise

        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (wait or on_status, {'wait': None}),
            (on_status, {'status': None})])

        return self.call(['scan', 'cabal'], [], opts, on_status)

    def scan_module(self, module, cabal = None, sandboxes = [], wait = False, on_status = None):
        if cabal is None:
            cabal = not sandboxes

        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (wait or on_status, {'wait': None}),
            (on_status, {'status': None})])

        return self.call(['scan', 'module'], [module], opts, on_status)

    def scan(self, projects = [], files = [], paths = [], wait = False, on_status = None):
        opts = concat_opts([
            (projects, {'project': projects}),
            (files, {'file': files}),
            (paths, {'path': paths}),
            (wait or on_status, {'wait': None}),
            (on_status, {'status': None})])

        return self.call(['scan'], [], opts, on_status)

    def rescan(self, projects = [], files = [], paths = [], wait = False, on_status = None):
        opts = concat_opts([
            (projects, {'project': projects}),
            (files, {'file': files}),
            (paths, {'path': paths}),
            (wait or on_status, {'wait': None}),
            (on_status, {'status': None})])

        return self.call(['rescan'], [], opts, on_status)

    def remove(self, cabal = False, sandboxes = [], projects = [], files = [], modules = []):
        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (projects, {'project': projects}),
            (files, {'file': files}),
            (modules, {'module': modules})])

        return self.call(['remove'], [], opts)

    def remove_all(self):
        return self.call(['remove'], [], {'all': None})

    def list_modules(self, cabal = False, sandboxes = None, projects = None, packages = None, source = False, standalone = False):
        opts = concat_opts([
            (cabal, {'cabal': None}),
            (sandboxes, {'sandbox': sandboxes}),
            (projects, {'project': projects}),
            (packages, {'package': packages}),
            (source, {'src': None}),
            (standalone, {'stand': None})])

        return parse_modules(self.call(['list', 'modules'], [], opts))

    def list_packages(self):
        return self.call(['list', 'packages'], [], {})

    def list_projects(self):
        return self.call(['list', 'projects'], [], {})

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
            (standalone, {'stand': None})])

        return parse_decls(self.call(['symbol'], [name] if name else [], opts))

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

        return parse_module(self.call(['module'], [], opts))

    def project(self, project):
        return self.call(['project'], [], {'project': project})

    def lookup(self, name, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return parse_decls(self.call(['lookup'], [name], opts))

    def whois(self, name, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return parse_decls(self.call(['whois'], [name], opts))

    def scope_modules(self, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return parse_modules(self.call(['scope', 'modules'], [], opts))

    def scope(self, file, sandbox = None, global_scope = False, prefix = None, find = None):
        opts = concat_opts([
            (True, {'file': file}),
            (sandbox, {'sandbox': sandbox}),
            (global_scope, {'global': None}),
            (prefix, {'prefix': prefix}),
            (find, {'find': find})])

        return parse_decls(self.call(['scope'], [], opts))

    def complete(self, input, file, sandbox = None):
        opts = {'file': file}

        if sandbox:
            opts.update({'sandbox': sandbox})

        return parse_decls(self.call(['complete'], [input], opts))

    def hayoo(self, query):
        return parse_decls(self.call(['hayoo'], [query], {}))

    def cabal_list(self, query = None):
        r = self.call(['cabal', 'list'], [query] if query else [], {})
        if r is None:
            return None
        return [parse_cabal_package(s) for s in r]

    def ghcmod_type(self, file, line, column = 1, sandbox = None):
        opts = concat_opts([
            (True, {'file': file}),
            (sandbox, {'sandbox': sandbox})])

        return self.call(['ghc-mod', 'type'], [str(line), str(column)], opts)

    def exit(self):
        self.call(['exit'], [], {})
        self.close()
