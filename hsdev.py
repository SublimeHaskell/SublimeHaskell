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
    return ["--sandbox"] if cabal == 'cabal' else ["--sandbox={0}".format(cabal)]

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
                self.socket.sendall(b'["link"]\n')
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
        thread = threading.Thread(
            target = self.accept)
        thread.start()

    def wait(self):
        self.connected.wait()

    def close(self):
        self.socket.close()
        if self.hsdev_socket:
            self.hsdev_socket.close()

    def is_connected(self):
        return self.connected.is_set()

    def call(self, command, args = [], opts = {}, on_status = None):
        if not self.is_connected():
            return None
        opts.update({'no-file': None})
        self.hsdev_socket.sendall('{0}\n'.format(json.dumps({
            'command': command,
            'args': args,
            'opts': opts })).encode())
        return self.receive_response(on_status)

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
