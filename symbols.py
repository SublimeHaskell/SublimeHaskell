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

    def position(self, column = True):
        """ Returns filename:line:column """
        if column:
            return ':'.join([self.filename if self.filename else '<no file>', str(self.line), str(self.column)])
        return ':'.join([self.filename if self.filename else '<no file>', str(self.line)])

    def set_file(self, other):
        if other is not None and type(other) == Location:
            self.filename = other.filename
            self.project = other.project

    def __str__(self):
        return self.to_string()

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

    def __str__(self):
        return self.to_string()

    def to_string(self):
        return '{0} in {1}'.format(self.package.package_id(), self.cabal)

    def is_null(self):
        return self.package is None

class OtherLocation(object):
    """
    Other module location
    """
    def __init__(self, source):
        self.source = source

    def __str__(self):
        return self.to_string()

    def to_string(self):
        return (self.source or "")

    def is_null(self):
        return self.source is None

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
        if self.by_source() and type(module_loc) == Location:
            self.location.set_file(module_loc)
        else:
            self.location = module_loc

    def full_name(self):
        return self.module.name + '.' + self.name

    def by_source(self):
        return type(self.location) == Location

    def by_cabal(self):
        return type(self.location) == InstalledLocation

    def by_hayoo(self):
        return type(self.location) == OtherLocation

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
    def __init__(self, module_name, exports = None, imports = [], declarations = {}, location = None, cabal = None, last_inspection_time = 0):
        super(Module, self).__init__('module', module_name, None, location)
        # List of strings
        if exports is not None:
            self.exports = exports[:]
        # Dictionary from module name to Import object
        self.imports = imports[:]
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

    def make_qualified(self):
        self.name = self.qualified_name()

    def suggest(self):
        """ Returns suggestion for this declaration """
        return ('{0}\t{1}'.format(self.name, self.module.name), self.name)

    def brief(self):
        return self.name

    def qualified_name(self):
        return '.'.join([self.module.name, self.name])

    def detailed(self):
        """ Detailed info for use in Symbol Info command """
        info = [
            self.brief(),
            '',
            'Imported from {0}'.format(self.module.name)]

        if self.docs:
            info.extend(['', self.docs])

        if self.by_source():
            info.append('')
            if self.location.project:
                info.append('Project: {0}'.format(self.location.project))
            info.append('Defined at: {0}'.format(self.location.position()))
        if self.by_cabal():
            info.append('')
            info.append('Package: {0}'.format(self.location.package.package_id()))
            info.append('Installed in: {0}'.format(self.location.cabal))

        return '\n'.join(info)

class Function(Declaration):
    """
    Haskell function declaration
    """
    def __init__(self, name, function_type, docs = None, location = None, module = None):
        super(Function, self).__init__(name, 'function', docs, location, module)
        self.type = function_type

    def suggest(self):
        return (u'{0} :: {1}\t{2}'.format(self.name, self.type, self.module.name), self.name)

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
        return (u'{0} {1}\t{2}'.format(self.name, ' '.join(self.args), self.module.name), self.name)

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
    same_filename = l.location and r.location and (l.location.filename == r.location.filename)
    nowhere = (not l.cabal) and (not l.location) and (not r.cabal) and (not r.location)
    return l.name == r.name and (same_cabal or same_filename or nowhere)

def same_declaration(l, r):
    """
    Returns true if l is same declaration as r
    """
    same_mod = l.module and r.module and same_module(l.module, r.module)
    nowhere = (not l.module) and (not r.module)
    return l.name == r.name and (same_mod or nowhere)

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


class CabalPackage(object):
    def __init__(self, name, synopsis = None, version = None, installed = [], homepage = None, license = None):
        self.name = name
        self.synopsis = synopsis
        self.default_version = version
        self.installed_versions = installed[:]
        self.homepage = homepage
        self.license = license

    def brief(self):
        return self.name

    def detailed(self):
        info = []
        info.append(self.brief())
        info.append('')
        if self.synopsis:
            info.append(self.synopsis)
            info.append('')
        if self.default_version:
            info.append('Last version: ' + self.default_version)
        if self.installed_versions:
            info.append('Installed versions: ' + ", ".join(self.installed_versions))
        if self.homepage:
            info.append('Homepage: ' + self.homepage)
        if self.license:
            info.append('License: ' + self.license)

        return '\n'.join(info)
