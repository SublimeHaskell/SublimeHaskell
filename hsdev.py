import os
import sys
import sublime
import subprocess
import threading
import json
import Queue
import time

import symbols
from sublime_haskell_common import create_process, log

class HsDev(object):
    def __init__(self):
        self.process = create_process(["hsdev.exe", "-json"])

    def die(self):
        self.process.terminate()

    def send_dict(self, d):
        self.process.stdin.write(json.dumps(d))
        self.process.stdin.write(os.linesep)
        self.process.stdin.flush()

    def send_cmd(self, cmd, **kwargs):
        kwargs.update({"cmd": cmd})
        kwargs = dict((k, v) for (k, v) in kwargs.items() if v is not None)
        self.send_dict(kwargs)

    def receive(self):
        s = self.process.stdout.readline()
        if s is not None:
            try:
                return json.loads(s)
            except:
                return None

    def receive_ok(self):
        d = self.receive()
        if d and 'result' in d and d['result'] == 'ok':
            return None
        else:
            log("result not ok: {0}".format(d))

    def receive_decls(self):
        d = self.receive()
        if d and 'declarations' in d:
            return [parse_declaration(decl) for decl in d['declarations']]
        return None

    def receive_modules(self):
        d = self.receive()
        if d and 'modules' in d:
            return [parse_module(m) for m in d['modules']]
        return None

    def load_cache(self, **kwargs):
        self.send_cmd('cache', load = True, **kwargs)
        self.receive_ok()

    def save_cache(self, **kwargs):
        self.send_cmd("cache", dump = True, **kwargs)
        self.receive_ok()

    def scan(self, **kwargs):
        self.send_cmd("scan", **kwargs)
        self.receive_ok()

    def find(self, name, **kwargs):
        self.send_cmd("find", name = name, **kwargs)
        return self.receive_decls()

    def list(self, **kwargs):
        self.send_cmd("list", **kwargs)
        d = self.receive()
        if d and 'params' in d:
            if 'modules' in d['params']:
                return d['params']['modules']
        return None

    def browse(self, module_name, **kwargs):
        self.send_cmd("browse", module = module_name, **kwargs)
        ms = self.receive_modules()
        return ms[0] if ms else None

    def goto(self, name, srcfile = None):
        self.send_cmd("goto", name = name, file = srcfile)
        return self.receive_decls()

    def info(self, name, srcfile = None):
        self.send_cmd("info", name = name, file = srcfile)
        return self.receive_decls()

    def complete(self, input_str, srcfile = None, module_name = None, cabal = None):
        self.send_cmd("complete", input = input_str, file = srcfile, module = module_name, cabal = cabal)
        return self.receive_decls()

    def dump(self, srcfile = None):
        if srcfile:
            self.send_cmd("dump", file = srcfile)
        else:
            self.send_cmd("dump", files = True)
        self.receive_ok()

    def exit(self):
        self.send_cmd("exit")
        self.die()



# class HsDev(object):
#     """
#     HsDev interactive process
#     """
#     def __init__(self):
#         self.process = create_process(["hsdev.exe", "-json"])
#         self.responses = Queue.Queue()
#         self.stop_event = threading.Event()
#         self.read_event = threading.Event()
#         self.thread = threading.Thread(
#             target=self.parse_output)
#         self.thread.daemon = True
#         self.thread.start()

#     def die(self):
#         self.process.terminate()
#         self.stop_event.set()

#     def parse_output(self):
#         while True:
#             print("waiting for read_event")
#             self.read_event.wait()
#             self.read_event.clear()
#             if self.stop_event.is_set():
#                 return
#             print("reading line")
#             s = self.process.stdout.readline()
#             if not s:
#                 return
#             try:
#                 self.responses.put(json.loads(s))
#             except:
#                 pass

#     def send_dict(self, d):
#         print('hsdev command: {0}'.format(d))

#         self.process.stdin.write(json.dumps(d))
#         self.process.stdin.write(os.linesep)
#         self.process.stdin.flush()

#         self.read_event.set()

#     def send_cmd(self, cmd, **kwargs):
#         kwargs.update({"cmd": cmd})
#         kwargs = dict((k, v) for (k, v) in kwargs.items() if v is not None)
#         self.send_dict(kwargs)

#     def receive(self):
#         try:
#             r = self.responses.get(timeout = 10)
#             if r and 'error' in r:
#                 raise RuntimeError(r['error'])
#             else:
#                 return r
#         except Queue.Empty:
#             return None

#     def receive_ok(self):
#         d = self.receive()
#         if d and 'result' in d and d['result'] == "ok":
#             return None
#         else:
#             raise RuntimeError('result not ok: {0}'.format(d))

#     def receive_decls(self):
#         d = self.receive()
#         if d and 'declarations' in d:
#             return [parse_declaration(decl) for decl in d['declarations']]
#         return None

#     def receive_modules(self):
#         d = self.receive()
#         if d and 'modules' in d:
#             return [parse_module(m) for m in d['modules']]
#         return None

#     def load_cache(self, **kwargs):
#         self.send_cmd("cache", load = True, **kwargs)
#         self.receive_ok()

#     def save_cache(self, **kwargs):
#         self.send_cmd("cache", dump = True, **kwargs)
#         self.receive_ok()

#     def scan(self, **kwargs):
#         self.send_cmd("scan", **kwargs)
#         self.receive_ok()

#     def find(self, name, **kwargs):
#         self.send_cmd("find", name = name, **kwargs)
#         return self.receive_decls()

#     def list(self, **kwargs):
#         self.send_cmd("list", **kwargs)
#         d = self.receive()
#         if d and 'params' in d:
#             if 'modules' in d['params']:
#                 return d['params']['modules']
#         return None

#     def browse(self, module_name, **kwargs):
#         self.send_cmd("browse", module = module_name, **kwargs)
#         ms = self.receive_modules()
#         return ms[0] if ms else None

#     def goto(self, name, srcfile = None):
#         self.send_cmd("goto", name = name, file = srcfile)
#         return self.receive_decls()

#     def info(self, name, srcfile = None):
#         self.send_cmd("info", name = name, file = srcfile)
#         return self.receive_decls()

#     def complete(self, input_str, srcfile = None, module_name = None, cabal = None):
#         self.send_cmd("complete", input = input_str, file = srcfile, module = module_name, cabal = cabal)
#         return self.receive_decls()

#     def dump(self, srcfile = None):
#         if srcfile:
#             self.send_cmd("dump", file = srcfile)
#         else:
#             self.send_cmd("dump", files = True)
#         self.receive_ok()

#     def exit(self):
#         self.send_cmd("exit")
#         self.die()

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
