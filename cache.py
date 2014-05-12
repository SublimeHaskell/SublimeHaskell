import json
import os
import sublime

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    import symbols
else:
    from SublimeHaskell.sublime_haskell_common import *
    import SublimeHaskell.symbols as symbols

CACHE_PATH = None
CABAL_CACHE_PATH = None
PROJECTS_CACHE_PATH = None

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
        symbols.Location: ('location', ['filename', 'line', 'column', 'project']),
        symbols.Symbol: ('symbol', ['what', 'name', 'docs', 'location']),
        symbols.Import: ('import', ['module', 'is_qualified', 'import_as']),
        symbols.Module: ('module', ['name', 'exports', 'imports', 'declarations', 'location', 'cabal', 'last_inspection_time']),
        symbols.Declaration: ('declaration', ['name', 'what', 'docs', 'location']),
        symbols.Function: ('function', ['name', 'type', 'docs', 'location']),
        symbols.TypeBase: ('typebase', ['name', 'what', 'context', 'args', 'definition', 'docs', 'location']),
        symbols.Type: ('type', ['name', 'context', 'args', 'definition', 'docs', 'location']),
        symbols.Newtype: ('newtype', ['name', 'context', 'args', 'definition', 'docs', 'location']),
        symbols.Data: ('data', ['name', 'context', 'args', 'definition', 'docs', 'location']),
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
        folders.append(''.join(filter(lambda c: c.isalpha() or c.isdigit(), base)))
    folders.reverse()
    return '.'.join(folders)

def dump_cabal_cache(database, cabal_name = None):
    if not cabal_name:
        cabal_name = current_cabal()
    formatted_json = None
    with database.get_cabal_modules(cabal_name) as cabal_modules:
        cabal_path = escape_path(cabal_name) if cabal_name != 'cabal' else 'cabal'
        cabal_json = os.path.join(CABAL_CACHE_PATH, cabal_path + '.json')
        formatted_json = encode_json(cabal_modules, indent = 2)
    with open(cabal_json, 'w') as f:
        f.write(formatted_json)

def dump_project_cache(database, project_path):
    formatted_json = None
    project_modules = database.get_project_modules(project_path)
    with database.files:
        project_json = os.path.join(PROJECTS_CACHE_PATH, escape_path(project_path) + '.json')
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

def load_project_cache(database, project_path):
    formatted_json = None
    project_json = os.path.join(PROJECTS_CACHE_PATH, escape_path(project_path) + '.json')
    if os.path.exists(project_json):
        with open(project_json, 'r') as f:
            formatted_json = f.read()
    if formatted_json:
        project_modules = decode_json(formatted_json)
        for m in project_modules.values():
            database.add_file(m.location.filename, m)

def plugin_loaded():
    global CACHE_PATH
    global CABAL_CACHE_PATH
    global PROJECTS_CACHE_PATH

    package_path = sublime_haskell_package_path()
    cache_path = sublime_haskell_cache_path()

if int(sublime.version()) < 3000:
    plugin_loaded()
