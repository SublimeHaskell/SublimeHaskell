import os
import sys
import sublime
import subprocess
import threading
import json
import Queue
import time

import symbols
from sublime_haskell_common import *

def call_hsdev_and_wait(arg_list, filename = None, cabal = None, callback = None, **popen_kwargs):
    cmd = ['hsdev', '--json'] + arg_list

    return call_and_wait_tool(cmd, 'hsdev', lambda s: json.loads(s), filename, callback, **popen_kwargs)

def hsdev(arg_list, on_response = None):
    r = call_hsdev_and_wait(arg_list, callback = on_response)
    if r is None:
        return
    if r and 'result' in r:
        if r['result'] == 'ok':
            return r
        elif r['result'] == 'error':
            log('hsdev returns error: {0} with details: {1}'.format(r['error'], r['details']))
            return None
    log('hsdev returns unknown response: {0}'.format(r))

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
    if 'status' in s:
        print(s['status'])
    else:
        log("Invalid status response: {0}".format(json.dumps(s)))

def scan(cabal = None, project = None, file = None, module = None, wait = False, on_status = None):
    opts = ['scan']
    if module:
        opts.extend(['module', module])
        if cabal:
            opts.extend(['-c', cabal])
    elif cabal:
        opts.extend(['cabal', '-c', cabal])
    elif project:
        opts.extend(['project', '-p', project])
    elif file:
        opts.extend(['file', file])
    else:
        opts.extend(['cabal'])

    if wait or on_status:
        opts.extend(['-w', '-s'])

    def onResponse(s):
        if on_status:
            on_status(s)

    if (not wait) and (on_status is not None): # async
        th = threading.Thread(
            target=hsdev,
            args=(opts, onResponse))
        th.start()
        return None
    else:
        return hsdev(opts, on_response = onResponse if wait else None)

def find(name):
    return parse_decls(hsdev(['find', name]))

def list():
    return hsdev(['list']).get('modules')

def browse(module_name):
    return parse_modules(hsdev(['browse', module_name]))[0]

def goto(name, file = None):
    return parse_decls(hsdev(['goto', name] + (['-f', file] if file else [])))

def info(name, file = None):
    return parse_decls(hsdev(['info', name] + (['-f', file] if file else [])))

def lookup(name, file):
    return parse_decls(hsdev(['lookup', file, name]))

def imports(ident, file):
    return hsdev(['import', file, ident]).get('imports')

def complete(input, file = None, module_name = None, cabal = None):
    if file:
        return parse_decls(hsdev(['complete', 'file', file, input]))
    return parse_decls(hsdev(
        ['complete', 'module', module_name, input] +
        (['-c', cabal] if cabal else [])))

def save_cache(path = None):
    p = [path] if path else []
    return hsdev(['cache', 'dump', 'all'] + p)

def load_cache(path = None):
    p = [path] if path else []
    return hsdev(['cache', 'load', 'all'] + p)

def exit():
    return hsdev(['exit'])

def parse_decls(s):
    if not s:
        return None
    if s and 'declarations' in s:
        return [parse_declaration(decl) for decl in s['declarations']]
    return None

def parse_modules(s):
    if not s:
        return None
    if s and 'modules' in s:
        return [parse_module(m) for m in s['modules']]
    return None

def parse_location(d):
    if not d:
        return None
    return symbols.Location(d['file'], d['line'], d['column'], d.get('project'))

def parse_cabal(d):
    if not d:
        return None
    if d == '<cabal>':
        return 'cabal'
    else:
        return d

def parse_import(d):
    if not d:
        return None
    return symbols.Import(d['module'], d['qualified'], d.get('as'))

def parse_module_head(d):
    return symbols.Module(
        d['name'],
        [], {}, {},
        parse_location(d.get('location')),
        parse_cabal(d.get('cabal')))

def parse_declaration(d, parse_module_info = True):
    try:
        m = None
        if 'module' in d and parse_module_info:
            m = parse_module_head(d['module'])

        if d['what'] == 'function':
            return symbols.Function(d['name'], d.get('type'), d.get('docs'), parse_location(d.get('location')), m)
        elif d['what'] == 'type':
            return symbols.Type(d['name'], d.get('ctx'), d.get('args'), d.get('definition'), d.get('docs'), parse_location(d.get('location')), m)
        elif d['what'] == 'newtype':
            return symbols.Newtype(d['name'], d.get('ctx'), d.get('args'), d.get('definition'), d.get('docs'), parse_location(d.get('location')), m)
        elif d['what'] == 'data':
            return symbols.Data(d['name'], d.get('ctx'), d.get('args'), d.get('definition'), d.get('docs'), parse_location(d.get('location')), m)
        elif d['what'] == 'class':
            return symbols.Class(d['name'], d.get('ctx'), d.get('args'), d.get('definition'), d.get('docs'), parse_location(d.get('location')), m)
        else:
            return None
    except:
        return None

def parse_module(d):
    return symbols.Module(
        d['name'],
        d.get('exports'),
        dict((i['module'], parse_import(i)) for i in d['imports']) if 'imports' in d else {},
        dict((decl['name'],parse_declaration(decl, False)) for decl in d['decls']) if 'decls' in d else {},
        parse_location(d.get('location')),
        parse_cabal(d.get('cabal')))

def test():
    p = HsDev()
    # time.sleep(10)
    p.load_cache(path = "e:")
    l = p.list()
    log(l)
