import threading
import sublime

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    from haskell_docs import haskell_docs
else:
    from SublimeHaskell.sublime_haskell_common import *
    from SublimeHaskell.haskell_docs import haskell_docs
    from functools import reduce

class Location(object):
    """
    Location in file at line
    """
    def __init__(self, filename, line, column, project = None):
        if not project and filename:
            project = get_cabal_project_dir_of_file(filename)
        self.project = project
        self.filename = filename
        self.line = line
        self.column = column

    def position(self):
        """ Returns filename:line:column """
        return ':'.join([self.filename if self.filename else '<no file>', str(self.line), str(self.column)])

    def set_file(self, other):
        if other is not None:
            self.filename = other.filename
            self.project = other.project

    def to_string(self):
        if self.line == 0 and self.column == 0:
            return self.filename
        return self.position()

    def is_null(self):
        return self.project is None and self.filename is None and self.line == 0 and self.column == 0

class Package(object):
    def __init__(self, name, version = None):
        self.name = name
        self.version = version

    def package_id(self):
        return '{0}-{1}'.format(self.name, self.version) if self.version is not None else self.name

def parse_package(package_id):
    if package_id is None:
        return None
    m = re.match('([\w\-]+)\-([\d\.]+)', package_id)
    if m:
        (name, version) = m.groups()
        return Package(name, version)
    m = re.match('([\w\-]+)', package_id)
    if m:
        (name, ) = m.groups()
        return Package(name)
    return None

class InstalledLocation(object):
    """
    Module location in cabal
    """
    def __init__(self, package, cabal = None):
        if not cabal:
            cabal = 'cabal'
        self.package = package
        self.cabal = cabal

    def to_string(self):
        return '{0} in {1}'.format(self.package.package_id(), self.cabal)

    def is_null(self):
        return self.package is None

class Symbol(object):
    """
    Haskell symbol: module, function, data, class etc.
    """
    def __init__(self, symbol_type, name, docs = None, location = None, module = None):
        self.what = symbol_type
        self.name = name
        self.module = module
        self.docs = docs
        self.location = location

        self.tags = {}

    def update_location(self, module_loc):
        """
        JSON contains only line + column
        This function used to merge module location, which contains all other info with line + column
        """
        if self.location and self.by_source():
            self.location.set_file(module_loc)
        else:
            self.location = module_loc

    def full_name(self):
        return self.module.name + '.' + self.name

    def by_source(self):
        return type(self.location) == Location

    def by_cabal(self):
        return type(self.location) == InstalledLocation

    def location_string(self):
        if not self.location:
            return None
        return self.location.to_string()

class Import(object):
    """
    Haskell import of module
    """
    def __init__(self, module_name, is_qualified = False, import_as = None, location = None):
        self.module = module_name
        self.is_qualified = is_qualified
        self.import_as = import_as
        self.location = location

    def dump(self):
        return self.__dict__

def module_location(filename):
    return Location(filename, 0, 0)

class Module(Symbol):
    """
    Haskell module symbol
    """
    def __init__(self, module_name, exports = [], imports = [], declarations = {}, location = None, cabal = None, last_inspection_time = 0):
        super(Module, self).__init__('module', module_name, None, location)
        # List of strings
        self.exports = exports
        # Dictionary from module name to Import object
        self.imports = [i for i in imports]
        for i in self.imports:
            if i.location:
                i.location.set_file(self.location)
        # Dictionary from name to Symbol
        self.declarations = declarations.copy()
        for d in self.declarations.values():
            d.update_location(self.location)

        for decl in self.declarations.values():
            decl.module = self

        # Cabal path or 'cabal'
        self.cabal = cabal

        # Time as from time.time()
        self.last_inspection_time = last_inspection_time

    def add_declaration(self, new_declaration):
        if not new_declaration.module:
            new_declaration.module = self
        new_declaration.update_location(self.location)
        if new_declaration.module != self:
            raise RuntimeError("Adding declaration to other module")
        self.declarations[new_declaration.name] = new_declaration

    def unalias(self, module_alias):
        """
        Unalias module import if any
        Returns list of unaliased modules
        """
        return [i.module for i in self.imports if i.import_as == module_alias]

class Declaration(Symbol):
    def __init__(self, name, decl_type = 'declaration', docs = None, location = None, module = None):
        super(Declaration, self).__init__(decl_type, name, docs, location, module)

    def suggest(self):
        """ Returns suggestion for this declaration """
        return (self.name, self.name)

    def brief(self):
        return self.name

    def qualified_name(self):
        return '.'.join([self.module.name, self.name])

    def detailed(self):
        """ Detailed info for use in Symbol Info command """
        info = [
            self.brief(),
            '',
            self.module.name]

        if self.docs:
            info.extend(['', self.docs])

        if self.by_source():
            info.append('')
            if self.location.project:
                info.append('Defined in {0} at {1}'.format(self.location.project, self.location.position()))
            else:
                info.append('Defined at {0}'.format(self.location.position()))
        if self.by_cabal():
            info.append('')
            info.append('Installed in {0} in package {1}'.format(self.location.cabal, self.location.package.package_id()))

        return '\n'.join(info)

class Function(Declaration):
    """
    Haskell function declaration
    """
    def __init__(self, name, function_type, docs = None, location = None, module = None):
        super(Function, self).__init__(name, 'function', docs, location, module)
        self.type = function_type

    def suggest(self):
        return (u'{0}\t{1}'.format(self.name, self.type), self.name)

    def brief(self):
        return u'{0} :: {1}'.format(self.name, self.type if self.type else u'?')

class TypeBase(Declaration):
    """
    Haskell type, data or class
    """
    def __init__(self, name, decl_type, context, args, definition = None, docs = None, location = None, module = None):
        super(TypeBase, self).__init__(name, decl_type, docs, location, module)
        self.context = context
        self.args = args
        self.definition = definition

    def suggest(self):
        return (u'{0}\t{1}'.format(self.name, ' '.join(self.args)), self.name)

    def brief(self):
        brief_parts = [self.what]
        if self.context:
            if len(self.context) == 1:
                brief_parts.append(u'{0} =>'.format(self.context[0]))
            else:
                brief_parts.append(u'({0}) =>'.format(', '.join(self.context)))
        brief_parts.append(self.name)
        if self.args:
            brief_parts.append(u' '.join(self.args))
        if self.definition:
            brief_parts.append(u' = {0}'.format(self.definition))
        return u' '.join(brief_parts)

class Type(TypeBase):
    """
    Haskell type synonym
    """
    def __init__(self, name, context, args, definition = None, docs = None, location = None, module = None):
        super(Type, self).__init__(name, 'type', context, args, definition, docs, location, module)

class Newtype(TypeBase):
    """
    Haskell newtype synonym
    """
    def __init__(self, name, context, args, definition = None, docs = None, location = None, module = None):
        super(Newtype, self).__init__(name, 'newtype', context, args, definition, docs, location, module)

class Data(TypeBase):
    """
    Haskell data declaration
    """
    def __init__(self, name, context, args, definition = None, docs = None, location = None, module = None):
        super(Data, self).__init__(name, 'data', context, args, definition, docs, location, module)

class Class(TypeBase):
    """
    Haskell class declaration
    """
    def __init__(self, name, context, args, definition = None, docs = None, location = None, module = None):
        super(Class, self).__init__(name, 'class', context, args, None, docs, location, module)

def update_with(l, r, default_value, f):
    """
    unionWith for Python, but modifying first dictionary instead of returning result
    """
    for k, v in r.items():
        if k not in l:
            l[k] = default_value[:]
        l[k] = f(l[k], v)
    return l

def same_module(l, r):
    """
    Returns true if l is same module as r, which is when module name is equal
    and modules defined in one file, in same cabal-dev sandbox or in cabal
    """
    same_cabal = l.cabal and r.cabal and (l.cabal == r.cabal)
    same_filename = l.by_source() and r.by_source() and (l.location.filename == r.location.filename)
    nowhere = (not l.cabal) and (not l.location) and (not r.cabal) and (not r.location)
    return l.name == r.name and (same_cabal or same_filename or nowhere)

def same_declaration(l, r):
    """
    Returns true if l is same declaration as r
    """
    same_mod = l.module and r.module and same_module(l.module, r.module)
    nowhere = (not l.module) and (not r.module)
    return l.name == r.name and (same_mod or nowhere)

class Database(object):
    """
    Database contains storages and indexes to allow fast access to module and symbol info in several storages
    Every info must be added to storages through methods of this class
    """
    def __init__(self):
        # Info is stored in several ways:

        # Dictionary from 'cabal' or cabal-dev path to modules dictionary, where
        # modules dictionary is dictionary from module name to Module
        # Every module is unique in such dictionary
        self.cabal_modules = LockedObject({})

        # Dictionary from filename to Module defined in this file
        self.files = LockedObject({})

        # Indexes: dictionary from module name to list of Modules
        self.modules = LockedObject({})

        # Indexes: dictionary from symbol name to list of Symbols to support Go To Definition
        self.symbols = LockedObject({})

    def get_cabal_modules(self, cabal = None):
        if not cabal:
            cabal = current_cabal()
        with self.cabal_modules as cabal_modules:
            if cabal not in cabal_modules:
                cabal_modules[cabal] = {}
        return LockedObject(self.cabal_modules.object[cabal], self.cabal_modules.object_lock)

    def get_project_modules(self, project_name):
        with self.files as files:
            return dict((f, m) for f, m in files.items() if m.location.project == project_name)

    def add_indexes_for_module(self, new_module):
        def append_return(l, r):
            l.append(r)
            return l

        with self.modules as modules:
            if new_module.name not in modules:
                modules[new_module.name] = []
            modules[new_module.name].append(new_module)

        with self.symbols as decl_symbols:
            update_with(decl_symbols, new_module.declarations, [], append_return)

    def remove_indexes_for_module(self, old_module):
        def remove_return(l, r):
            return [x for x in l if not same_declaration(x, r)]

        with self.modules as modules:
            if old_module.name in modules:
                modules[old_module.name] = [m for m in modules[old_module.name] if not same_module(old_module, m)]

        with self.symbols as decl_symbols:
            update_with(decl_symbols, old_module.declarations, [], remove_return)

    def add_indexes_for_declaration(self, new_declaration):
        with self.symbols as decl_symbols:
            if new_declaration.name not in decl_symbols:
                decl_symbols[new_declaration.name] = []
            decl_symbols[new_declaration.name].append(new_declaration)

    def remove_indexes_for_declaration(self, old_declaration):
        with self.symbols as decl_symbols:
            if old_declaration.name in decl_symbols:
                decl_symbols[old_declaration.name] = [d for d in decl_symbols[old_declaration.name] if not same_declaration(d, old_declaration)]

    def add_module(self, new_module, cabal = None):
        """
        Adds module and updates indexes
        """
        if not cabal:
            if new_module.cabal:
                cabal = new_module.cabal
            else:
                cabal = current_cabal()
                new_module.cabal = cabal

        with self.cabal_modules as cabal_modules:
            if cabal not in cabal_modules:
                cabal_modules[cabal] = {}
            if new_module.name in cabal_modules[cabal]:
                old_module = cabal_modules[cabal][new_module.name]
                self.remove_indexes_for_module(old_module)
                del cabal_modules[cabal][new_module.name]
            if new_module.name not in cabal_modules[cabal]:
                cabal_modules[cabal][new_module.name] = new_module
                self.add_indexes_for_module(new_module)

    def add_file(self, filename, file_module):
        """
        Adds module defined in file and updates indexes
        """
        with self.files as files:
            if filename in files:
                old_module = files[filename]
                self.remove_indexes_for_module(old_module)
                del files[filename]
            if filename not in files:
                files[filename] = file_module
                self.add_indexes_for_module(file_module)

    def add_declaration(self, new_declaration, module):
        """
        Adds declaration to module
        """
        def add_decl_to_module():
            if new_declaration.name in module.declarations:
                self.remove_indexes_for_declaration(module.declarations[new_declaration.name])
            module.add_declaration(new_declaration)
            self.add_indexes_for_declaration(new_declaration)

        if module.location:
            with self.files as files:
                if module.location.filename not in files:
                    raise RuntimeError("Can't add declaration: no file {0}".format(module.location.filename))
                add_decl_to_module()
        elif module.cabal:
            if module.name not in self.cabal_modules.object[module.cabal]:
                raise RuntimeError("Can't add declaration: no module {0}".format(module.name))
            add_decl_to_module()
        else:
            raise RuntimeError("Can't add declaration: no module {0}".format(module.name))



def is_within_project(module, project):
    """
    Returns whether module defined within project specified
    """
    if module.location:
        return module.location.project == project
    return False

def is_within_cabal(module, cabal = None):
    """
    Returns whether module loaded from cabal specified
    If cabal is None, used current cabal
    """
    if not cabal:
        cabal = current_cabal()
    return module.cabal == cabal

def is_by_sources(module):
    """
    Returns whether module defined by sources
    """
    return module.location is not None

def flatten(lsts):
    return reduce(lambda l, r: list(l) + list(r), lsts)

def get_source_modules(modules, filename = None):
    """
    For list of modules with same name returns modules, which is defined by sources
    Prefer module in same project as filename if specified
    """
    project = get_cabal_project_dir_of_file(filename) if filename else None

    candidates = flatten([
        filter(lambda m: is_within_project(m, project), modules),
        filter(is_by_sources, modules)])

    if candidates:
        return candidates[0]
    return None

def get_visible_module(modules, filename = None, cabal = None):
    """
    For list of modules with same name returns module, which is
    1. Defined in same project as filename
    2. Defined in cabal
    3. None
    """
    project = get_cabal_project_dir_of_file(filename) if filename else None

    candidates = flatten([
        filter(lambda m: is_within_project(m, project), modules),
        filter(lambda m: is_within_cabal(m, cabal), modules)])

    if candidates:
        return candidates[0]
    return None

def get_preferred_module(modules, filename = None, cabal = None):
    """
    For list of modules with same name returns module, which is
    1. Defined in same project as filename
    2. Defined in cabal
    3. Defined by sources
    4. Other modules
    Returns None if modules is empty
    """
    if filename:
        project = get_cabal_project_dir_of_file(filename)

    candidates = flatten([
        filter(lambda m: is_within_project(m, project), modules),
        filter(lambda m: is_within_cabal(m, cabal), modules),
        filter(is_by_sources, modules),
        modules])

    if candidates:
        return candidates[0]
    return None

def declarations_modules(decls, select_module = None):
    """
    Reduce list of declarations to dictionary (module_name => select_module(list of modules))
    """
    def add_module_to_dict(d, decl):
        if decl.module.name not in d:
            d[decl.module.name] = []
        d[decl.module.name].append(decl.module)
        return d

    if not select_module:
        select_module = lambda l: l

    result = reduce(add_module_to_dict, decls, {})
    return dict((k, select_module(ms)) for k, ms in result.items() if select_module(ms) is not None)

def is_imported_module(in_module, m, qualified_name = None):
    """
    Returns whether 'm' is imported from 'in_module'
    If 'qualified_name' specified, 'm' must be 'qualified_name' or imported as 'qualified_name'
    """
    if qualified_name:
        for i in in_module.imports:
            if m.name == i:
                cur_import = i
                return cur_import.module == qualified_name or cur_import.import_as == qualified_name
        return False
    else:
        for i in in_module.imports:
            if m.name == i:
                return (not i.is_qualified)
        # Return True also on Prelude
        return m.name == 'Prelude'

def is_this_module(this_module, m):
    """
    Returns whether 'm' is the same as 'this_module'
    """
    # Same source
    if this_module.location and m.location and this_module.location.filename == m.location.filename:
        return True
    # Same name and cabal
    if this_module.cabal and m.cabal and this_module.cabal == m.cabal and this_module.name == m.name:
        return True
    return False
