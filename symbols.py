import threading

class Location(object):
    """
    Location in file at line
    """
    def __init__(self, filename, line):
        self.filename = filename
        self.line = line

class Symbol(object):
    """
    Haskell symbol: module, function, data, class etc.
    """
    def __init__(self, symbol_type, name, module = None, docs = None, location = None):
        self.what = symbol_type
        self.name = name
        self.module = module
        self.docs = docs
        self.location = location

class Import(object):
    """
    Haskell import of module
    """
    def __init__(self, module, qualified = None):
        self.module = module
        self.qualified = qualified

class Module(Symbol):
    """
    Haskell module symbol
    """
    def __init__(self, module_name, exports = [], imports = {}, declarations = {}, filename = None):
        super(Module, self).__init__('module', module_name, None, None, Location(filename, 1) if filename else None)
        self.exports = exports
        self.imports = imports
        self.declarations = declarations

class Function(Symbol):
    """
    Haskell function
    """
    def __init__(self, name, module = None, function_type = None, docs = None, location = None):
        super(Function, self).__init__('function', name, module, docs, location)
        self.type = function_type
        self.module = module

class Class(Symbol):
    """
    Haskell class
    class context => name args where
    """
    def __init__(self, name, module = None, context = None, args = [], docs = None, location = None):
        super(Class, self).__init__('class', name, module, docs, location)
        self.context = context
        self.args = args

class Data(Symbol):
    """
    Haskell data
    data context => name args =
    """
    def __init__(self, name, module = None, context = None, args = [], docs = None, location = None):
        super(Data, self).__init__('data', name, module, docs, location)
        self.context = context
        self.args = args

def update_with(l, r, default_value, f):
    """
    unionWith for Python, but modifying first dictionary instead of returning result
    """
    for k, v in r.items():
        if k not in l:
            l[k] = default_value
        l[k] = f(l[k], v)
    return l

class Storage(object):
    def __init__(self):
        # Info is stored in several ways:

        # Dictionary from module name to list of Modules,
        #   where every declaration is dictionary from symbol name to Symbol
        self.modules = {}
        self.modules_lock = threading.Lock()

        # Dictionary from file to Module for project files
        self.files = {}
        self.files_lock = threading.Lock()

        # And dictionary from symbol name to list of Symbols to support Go To Definition
        self.symbols = {}
        self.symbols_lock = threading.Lock()

    def add_module(new_module):
        """
        Places module and its symbols in dictionaries
        """
        with self.modules_lock:
            if new_module.name not in self.modules:
                self.modules[new_module.name] = []
            self.modules[new_module.name].append(new_module)

        if new_module.location:
            with self.files_lock:
                self.files[new_module.location.filename] = new_module

        with self.symbols_lock:
            update_with(self.symbols, new_module.declarations, [], lambda l, r: l.append(r))
