import json
import os

from sublime_haskell_common import *
import symbols

CACHE_PATH = os.path.join(PACKAGE_PATH, 'cache')
CABAL_CACHE_PATH = os.path.join(CACHE_PATH, 'cabal')
PROJECTS_CACHE_PATH = os.path.join(CACHE_PATH, 'projects')

def swap_dict(d):
    """
    {key => (item, value)} => {item => (key, value)}
    """
    return dict((v[0], (k, v[1])) for k, v in d.items())

def as_object(serializers, dct):
    """
    Parse JSON as object
    Serializers is dictionary name => serializer, where
    serializer is tuple (type, list of field in order of appearance in constructor)
    """
    sers = swap_dict(serializers)
    if '__type__' in dct:
        if dct['__type__'] in sers:
            (load_type, fields) = sers[dct['__type__']]
            return load_type(*[dct.get(f) for f in fields])
        else:
            raise RuntimeError("Unknown type '{0}'".format(dct['__type__']))
    else:
        return dct

class SymbolsEncoder(json.JSONEncoder):
    def __init__(self, serializers = None, **kwargs):
        super(SymbolsEncoder, self).__init__(**kwargs)
        self.serializers = serializers

    def default(self, obj):
        if type(obj) in self.serializers:
            (name, args) = self.serializers[type(obj)]
            result = dict((k, v) for k, v in obj.__dict__.items() if k in args)
            result.update({'__type__': name})
            return result
        return json.JSONEncoder.default(self, obj)

def symbol_serializers():
    return {
        symbols.Location: ('location', ['filename', 'line', 'column', 'project', 'modified_time']),
        symbols.Symbol: ('symbol', ['what', 'name', 'docs', 'location']),
        symbols.Import: ('import', ['module', 'is_qualified', 'import_as']),
        symbols.Module: ('module', ['name', 'exports', 'imports', 'declarations', 'location', 'cabal']),
        symbols.Declaration: ('declaration', ['name', 'what', 'docs', 'location']),
        symbols.Function: ('function', ['name', 'type', 'docs', 'location']),
        symbols.TypeBase: ('typebase', ['name', 'what', 'context', 'args', 'docs', 'location']),
        symbols.Type: ('type', ['name', 'context', 'args', 'docs', 'location']),
        symbols.Newtype: ('newtype', ['name', 'context', 'args', 'docs', 'location']),
        symbols.Data: ('data', ['name', 'context', 'args', 'docs', 'location']),
        symbols.Class: ('class', ['name', 'context', 'args', 'docs', 'location']) }

def encode_json(obj, **kwargs):
    return json.dumps(obj, cls = SymbolsEncoder, serializers = symbol_serializers(), **kwargs)

def decode_json(s):
    return json.loads(s, object_hook = lambda v: as_object(symbol_serializers(), v))

def escape_path(path):
    path = os.path.abspath(os.path.normcase(path))
    folders = []
    (base, name) = os.path.split(path)
    while name:
        folders.append(name)
        (base, name) = os.path.split(base)
    if base:
        folders.append(filter(lambda c: c.isalpha() or c.isdigit(), base))
    folders.reverse()
    return '.'.join(folders)

def dump_cabal_cache(database, cabal_name = None):
    if not cabal_name:
        cabal_name = current_cabal()
    formatted_json = None
    cabal_modules = database.get_cabal_modules(cabal_name)
    with database.cabal_modules_lock:
        cabal_path = escape_path(cabal_name) if cabal_name != 'cabal' else 'cabal'
        cabal_json = os.path.join(CABAL_CACHE_PATH, cabal_path + '.json')
        formatted_json = encode_json(cabal_modules, indent = 2)
    with open(cabal_json, 'w') as f:
        f.write(formatted_json)

def dump_project_cache(database, project_name):
    formatted_json = None
    project_modules = database.get_project_modules(project_name)
    with database.files_lock:
        project_json = os.path.join(PROJECTS_CACHE_PATH, escape_path(project_name) + '.json')
        formatted_json = encode_json(project_modules, indent = 2)
    with open(project_json, 'w') as f:
        f.write(formatted_json)

def load_cabal_cache(database, cabal_name = None):
    if not cabal_name:
        cabal_name = current_cabal()
    formatted_json = None
    cabal_path = escape_path(cabal_name) if cabal_name != 'cabal' else 'cabal'
    cabal_json = os.path.join(CABAL_CACHE_PATH, cabal_path + '.json')
    if os.path.exists(cabal_json):
        with open(cabal_json, 'r') as f:
            formatted_json = f.read()
    if formatted_json:
        cabal_modules = decode_json(formatted_json)
        for m in cabal_modules.values():
            database.add_module(m, cabal_name)

def load_project_cache(database, project_name):
    formatted_json = None
    project_json = os.path.join(PROJECTS_CACHE_PATH, escape_path(project_name) + '.json')
    if os.path.exists(project_json):
        with open(project_json, 'r') as f:
            formatted_json = f.read()
    if formatted_json:
        project_modules = decode_json(formatted_json)
        for m in project_modules.values():
            database.add_file(m.location.filename, m)

def plugin_loaded():
    if not os.path.exists(CACHE_PATH):
        os.mkdir(CACHE_PATH)
        os.mkdir(CABAL_CACHE_PATH)
        os.mkdir(PROJECTS_CACHE_PATH)

if int(sublime.version()) < 3000:
    plugin_loaded()
