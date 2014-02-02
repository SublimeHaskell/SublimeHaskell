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
            return {'response' : s}

    log(' '.join(cmd))
    ret = call_and_wait_tool(cmd, 'hsdev', parse_response, filename, on_line if callback else None, **popen_kwargs)
    if ret is not None:
        result = ret

    return result

def hsdev(arg_list, on_response = None):
    if get_setting_async('enable_hsdev') != True:
        return None

    r = call_hsdev_and_wait(arg_list, callback = on_response)
    if r is None:
        return None
    if r and 'error' in r:
        log('hsdev returns error: {0} with details: {1}'.format(r['error'], r['details']))
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
        log('hsinspect must specify module, file or cabal')
        return None

    for opt in ghc_opts:
        cmd.extend(['-g', opt])

    r = call_and_wait_tool(cmd, 'hsinspect', lambda s: json.loads(s), file, None)
    if r:
        if 'error' in r:
            log('hsinspect returns error: {0}'.format(r['error']))
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
    return hsdev(['server', 'start'] + if_some(port, ['--port', str(port)]) + if_some(cache, ['--cache', cache]) + if_some(log, ['--log', log])) is not None

def link(port = None, parent = None):
    return hsdev(['link'] + if_some(port, ['--port', str(port)]) + if_some(parent, ['--parent', parent])) is not None

def stop(port = None):
    return hsdev(['server', 'stop'] + if_some(port, ['--port', str(port)])) is not None

def scan(cabal = None, projects = [], files = [], paths = [], modules = [], wait = False, on_status=None):
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

    return hsdev(opts, on_response = onResponse if wait else None)

def rescan(projects = [], files = [], paths = [], wait = False, on_status = None):
    opts = ['rescan']
    args = [['--project', p] for p in projects] + [['-f', f] for f in files] + [['-p', p] for p in paths]

    if not args:
        log('hsdev.rescan: must specify at least one param')
        return None

    opts.extend(list(reduce(lambda x, y: x + y, args)))

    if wait or on_status:
        opts.extend(['-w', '-s'])

    opts.extend(get_ghc_opts_args(filename = file))

    def onResponse(s):
        if on_status:
            on_status(s)

    return hsdev(opts, on_response = onResponse if wait else None)

def remove(cabal = None, project = None, file = None, module = None):
    return hsdev(
        ['remove'] +
        cabal_path(cabal) +
        if_some(project, ['--project', project]) +
        if_some(file, ['-f', file]) +
        if_some(module, ['-m', module]))

def remove_all():
    return hsdev(['remove', '-a'])

def list_modules(cabal = None, project = None, source = False, standalone = False):
    return parse_modules(
        hsdev(
            ['list', 'modules'] +
            cabal_path(cabal) +
            if_some(project, ['--project', project]) +
            (['--src'] if source else []) +
            (['--stand'] if standalone else [])))

def list_projects():
    return hsdev(['list', 'projects'])

def symbol(name = None, project = None, file = None, module = None, package = None, cabal = None, source = False, standalone = False, prefix = None, find = None):
    return parse_decls(
        hsdev(
            (['symbol', name] if name else ['symbol']) +
            if_some(project, ['--project', project]) +
            if_some(file, ['-f', file]) +
            if_some(module, ['-m', module]) +
            if_some(package, ['--package', package]) +
            cabal_path(cabal) +
            (['--src'] if source else []) +
            (['--stand'] if standalone else []) +
            if_some(prefix, ['--prefix', prefix]) +
            if_some(find, ['--find', find])))

def module(name = None, package = None, project = None, file = None, cabal = None, source = False):
    return parse_module(
        hsdev(
            ['module'] +
            if_some(name, ['-m', name]) +
            if_some(package, ['--package', package]) +
            if_some(project, ['--project', project]) +
            cabal_path(cabal) +
            if_some(file, ['-f', file]) +
            (['--src'] if source else [])))

def project(projects):
    return hsdev(['project'] + projects)

def lookup(name, file, cabal = None):
    return parse_decls(
        hsdev(
            ['lookup', name, '-f', file] + cabal_path(cabal)))

def whois(name, file, cabal = None):
    return parse_decls(
        hsdev(
            ['whois', name, '-f', file] + cabal_path(cabal)))

def scope_modules(file, cabal = None):
    return parse_modules(
        hsdev(
            ['scope', 'modules', '-f', file] + cabal_path(cabal)))

def scope(file, cabal = None, global_scope = False, prefix = None, find = None):
    return parse_decls(
        hsdev(
            ['scope', '-f', file] +
            cabal_path(cabal) +
            (['--global'] if global_scope else []) +
            if_some(prefix, ['--prefix', prefix]) +
            if_some(find, ['--find', find])))

def complete(input, file, cabal = None):
    return parse_decls(
        hsdev(
            ['complete', input, '-f', file] + cabal_path(cabal)))

def dump(cabal = None, projects = [], files = [], path = None, file = None):
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

    r = hsdev(opts)
    if r:
        return parse_database(r)
    else:
        return r

def load(path = None, file = None, data = None):
    return hsdev(
        ['load'] +
        if_some(path, ['-p', path]) +
        if_some(file, ['-f', file]) +
        if_some(data, ['--data', data]))

def exit():
    return hsdev(['exit'])

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
        [], {}, {},
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
        log('Error pasring declaration: {0}'.format(e))
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

def test():
    p = HsDev()
    # time.sleep(10)
    p.load_cache(path = "e:")
    l = p.list()
    log(l)

class HsDevHolder(object):
    def __init__(self, port = 4567, cache = None):
        super(HsDevHolder, self).__init__()
        self.port = port
        self.cache = cache
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.started_event = threading.Event()

    def run_hsdev(self, tries = 10):
        self.start_hsdev()
        self.link_hsdev(tries = tries)

    def start_hsdev(self):
        start(port = self.port, cache = self.cache)

    def link_hsdev(self, tries = 10):
        for n in range(0, tries):
            try:
                log('connecting to hsdev server...')
                self.socket.connect(('127.0.0.1', self.port))
                log('connected to hsdev server')
                self.socket.sendall(b'["link"]\n')
                self.started_event.set()
                log('hsdev server started')
                return
            except:
                log('failed to connect to hsdev server, wait for a while')
                time.sleep(0.1)

    # Wait until linked
    def wait_hsdev(self, timeout = 60):
        return self.started_event.wait(timeout)

hsdev_holder = None

def start_server(port = None, cache = None):
    global hsdev_holder
    hsdev_holder = HsDevHolder(port, cache)
    hsdev_holder.run_hsdev()
