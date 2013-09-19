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

    result = None

    def on_line(l):
        if l:
            if 'status' in l:
                callback(l)
            else:
                result = l

    def parse_response(s):
        return {} if s.isspace() else json.loads(s)

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
        log('hsdev returns nothing')
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
    args = ['--sandbox']
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

def symbol(name, cabal = None, project = None, file = None, module = None, source = False, standalone = False):
    return parse_decls(
        hsdev(
            ['symbol', name] +
            cabal_path(cabal) +
            if_some(project, ['-p', project]) +
            if_some(file, ['-f', file]) +
            if_some(module, ['-m', module]) +
            (['--src'] if source else []) +
            (['--stand'] if standalone else [])))

def module(name = None, project = None, file = None, cabal = None):
    return parse_module(
        hsdev(
            ['module'] +
            if_some(name, ['-m', name]) +
            cabal_path(cabal) +
            if_some(project, ['-p', project]) +
            if_some(file, ['-f', file])))

def project(projects):
    return hsdev(['project'] + projects)

def whois(name, file, cabal = None):
    return parse_module_declaration(
        hsdev(
            ['whois', name, '-f', file] + cabal_path(cabal)))

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
    return symbols.Location(
        get_value(d, 'file'),
        get_value(p, 'line', 0),
        get_value(p, 'column', 0),
        get_value(d, 'project'))

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

        if decl.location:
            decl.location.set_file(loc)

        decl.module = m

        return decl
    except:
        return None

def parse_module(d):
    return symbols.Module(
        d['name'],
        d.get('exports'),
        dict((i['name'], parse_import(i)) for i in d['imports']) if 'imports' in d else {},
        dict((decl['name'],parse_declaration(decl)) for decl in d['declarations']) if 'declarations' in d else {},
        parse_location(d.get('location')),
        parse_cabal(d.get('location')))

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
