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

def call_hsdev_and_wait(arg_list, filename = None, cabal = None):
    try:
        exit_code, out, err = call_and_wait(['hsdev'] + arg_list)

        return crlf2lf(out)
    except OSError as e:
        return None

def hsdev(arg_list):
    s = call_hsdev_and_wait(arg_list)
    return (json.loads(s) if s else None)

def load_cache(path = None):
    p = ['-path', path] if path else []
    return hsdev(['cache', '-load'] + p)

def save_cache(path = None):
    p = ['-path', path] if path else []
    return hsdev(['cache', '-dump'] + p)

def scan(cabal = None, project = None, file = None):
    if cabal:
        return hsdev(['scan', '-cabal', cabal])
    if project:
        return hsdev(['scan', '-project', project])
    if file:
        return hsdev(['scan', '-file', file])
    return hsdev(['scan', '-cabal'])

def find(name):
    return parse_decls(hsdev(['find', name]))

def list():
    return hsdev(['list'])

def browse(module_name):
    return parse_modules(hsdev(['browse', module_name]))[0]

def goto(name, file = None):
    return parse_decls(hsdev(['goto', name] + (['-file', file] if file else [])))

def info(name, file = None):
    return parse_decls(hsdev(['info', name] + (['-file', file] if file else [])))

def complete(input, file = None, module_name = None, cabal = None):
    if file:
        return parse_decls(hsdev(['complete', input, '-file', file]))
    return parse_decls(hsdev(
        ['complete', input] +
        (['-module', module_name] if module_name else []) +
        (['-cabal', cabal] if cabal else [])))

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
