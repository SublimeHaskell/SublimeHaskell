import os
import sys
import sublime
import sublime_plugin
import subprocess
import threading
import json
import Queue
import time

import symbols
from sublime_haskell_common import *
import autocomplete

def call_hsdev_and_wait(arg_list, filename = None, cabal = None, callback = None, **popen_kwargs):
    cmd = ['hsdev', '--json'] + arg_list

    result = {}

    def on_line(l):
        if l and 'status' in l:
            callback(l)
        else:
            result.update(l)

    def parse_response(s):
        return {} if s.isspace() else json.loads(s)

    log(' '.join(cmd))
    ret = call_and_wait_tool(cmd, 'hsdev', parse_response, filename, on_line if callback else None, **popen_kwargs)
    if ret:
        result.update(ret)

    return result

def hsdev(arg_list, on_response = None):
    if get_setting_async('enable_hsdev') != True:
        return None

    r = call_hsdev_and_wait(arg_list, callback = on_response)
    if r is None:
        log('hsdev returns nothing')
        return None
    if r and 'result' in r:
        if r['result'] == 'error':
            log('hsdev returns error: {0} with details: {1}'.format(r['error'], r['details']))
        return r
    log('hsdev returns unknown response: {0}'.format(r))
    return None

def if_some(x, lst):
    return lst if x is not None else []

def cabal_path(cabal):
    if not cabal:
        return []
    args = ['-c']
    if cabal != 'cabal':
        args.append(cabal)
    return args

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

def start(port = None):
    return hsdev(['server', 'start'] + if_some(port, ['--port', str(port)])) is not None

def stop(port = None):
    return hsdev(['server', 'stop'] + if_some(port, ['--port', str(port)])) is not None

def scan(cabal = None, project = None, file = None, path = None, module = None, projects = False, wait = False, on_status = None):
    opts = ['scan']
    if module:
        opts.extend(['module', module])
        if cabal:
            opts.extend(['-c', cabal])
    elif cabal:
        opts.extend(['cabal'] + cabal_path(cabal))
    elif project:
        opts.extend(['project', '-p', project])
    elif file:
        opts.extend(['file', file])
    elif path:
        opts.extend(['path', path])
        if projects:
            opts.extends(['--projects'])
    else:
        opts.extend(['cabal'])

    if wait or on_status:
        opts.extend(['-w', '-s'])

    opts.extend(get_ghc_opts_args(filename = file, cabal = cabal))

    def onResponse(s):
        if on_status:
            on_status(s)

    return hsdev(opts, on_response = onResponse if wait else None)

def rescan(project = None, file = None, directory = None, wait = False, on_status = None):
    opts = ['rescan']
    if project:
        opts.extend(['-p', project])
    elif file:
        opts.extend(['-f', file])
    elif directory:
        opts.extend(['-d', directory])
    else:
        log('hsdev.rescan: must specify at least one param')
        return None

    if wait or on_status:
        opts.extend(['-w', '-s'])

    opts.extend(get_ghc_opts_args(filename = file))

    def onResponse(s):
        if on_status:
            on_status(s)

    return hsdev(opts, on_response = onResponse if wait else None)

def clean(cabal = None, project = None, file = None, module = None):
    return hsdev(['clean'] + cabal_path(cabal) + if_some(project, ['-p', project]) + if_some(file, ['-f', file]) + if_some(module, ['-m', module]))

def find(name, cabal = None, project = None, file = None, module = None, source = False, standalone = False):
    return parse_decls(hsdev(['find', name] + cabal_path(cabal) + if_some(project, ['-p', project]) + if_some(file, ['-f', file]) + if_some(module, ['-m', module]) + (['--src'] if source else []) + (['--stand'] if standalone else [])))

def list(cabal = None, project = None, source = False, standalone = False):
    return hsdev(['list'] + cabal_path(cabal) + if_some(project, ['-p', project]) + (['--src'] if source else []) + (['--stand'] if standalone else [])).get('modules', "").splitlines()

def browse(module = None, cabal = None, project = None, file = None):
    return parse_modules(hsdev(['browse'] + if_some(module, ['-m', module]) + cabal_path(cabal) + if_some(project, ['-p', project]) + if_some(file, ['-f', file])))[0]

def goto(name, file = None):
    return parse_decls(hsdev(['goto', name] + if_some(file, ['-f', file])))

def info(name, file = None):
    return parse_decls(hsdev(['info', name] + if_some(file, ['-f', file])))

def lookup(name, file):
    return parse_decls(hsdev(['lookup', file, name]))

def complete(input, file = None, module_name = None, cabal = None):
    if file:
        return parse_decls(hsdev(['complete', 'file', file, input]))
    return parse_decls(hsdev(
        ['complete', 'module', module_name, input] +
        cabal_path(cabal)))

def projects():
    return hsdev(['project']).get('projects', "").splitlines()

def project(p):
    return hsdev(['project', p])

def save_cache(path = None):
    p = ['--path', path] if path else []
    return hsdev(['cache', 'dump'] + p)

def load_cache(path = None, wait = False):
    return hsdev(['cache', 'load'] + if_some(path, ['--path', path]) + if_some(wait, ['-w']))

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
    return symbols.Import(d['module'], d['qualified'], d.get('as'), parse_location(d.get('location')))

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

class SublimeHaskellInsertImport(sublime_plugin.TextCommand):
    """
    Insert 'import <module>' for symbol selected
    """
    def run(self, edit):
        (module_word, ident, _) = autocomplete.get_qualified_symbol_at_region(self.view, self.view.sel()[0])
        full_name = '{0}.{1}'.format(module_word, ident) if module_word else ident

        self.current_file_name = self.view.file_name()
        self.edit = edit

        is_visible = info(full_name, file = self.current_file_name) is not None
        if is_visible:
            show_status_message('Symbol {0} already imported'.format(full_name))
            return

        self.candidates = lookup(full_name, self.current_file_name)

        self.view.window().show_quick_panel([[decl.module.name, decl.brief()] for decl in self.candidates], self.on_import_selected)

    def on_import_selected(self, idx):
        if idx == -1:
            return

        candidate = self.candidates[idx]

        cur_module = browse(file = self.current_file_name)
        imports = sorted(cur_module.imports.values(), key = lambda i: i.location.line)
        after = [i for i in imports if i.module > candidate.module.name]

        insert_line = 0
        insert_gap = False

        if len(after) > 0:
            # Insert before after[0]
            insert_line = after[0].location.line - 1
        elif len(imports) > 0:
            # Insert as last import
            insert_line = imports[-1].location.line
        elif len(cur_module.declarations) > 0:
            # Insert before first declaration
            insert_line = min([decl.location.line for decl in cur_module.declarations.values()]) - 1
            insert_gap = True
        else:
            # Insert at the end of file
            insert_line = self.view.rowcol(self.view.size())[0]

        insert_text = 'import {0}\n'.format(candidate.module.name) + ('\n' if insert_gap else '')

        pt = self.view.text_point(insert_line, 0)
        self.view.insert(self.edit, pt, insert_text)
