# -*- coding: UTF-8 -*-

from functools import total_ordering, reduce
import html
import json
import os.path
import re
import sublime

import SublimeHaskell.internals.unicode_opers as UnicodeOpers


@total_ordering
class Position(object):
    def __init__(self, line, column):
        self.line = line
        self.column = column

    def __str__(self):
        return self.to_string()

    def __unicode__(self):
        return self.to_string()

    def to_string(self):
        if self.column is None:
            return str(self.line)
        return u':'.join([str(self.line), str(self.column)])

    def __eq__(self, other):
        return self.line == other.line and self.column == other.column

    def __lt__(self, other):
        return self.line < other.line or (self.line == other.line and self.column < other.column)

    def to_zero_based(self):
        self.line = self.line - 1
        self.column = self.column - 1
        return self

    def from_zero_based(self):
        self.line = self.line + 1
        self.column = self.column + 1
        return self

    def to_point(self, view):
        return view.text_point(self.line, self.column)

    @staticmethod
    def from_point(view, point):
        (line, col) = view.rowcol(point)
        return Position(int(line), int(col))

    @staticmethod
    def from_str(pt_str):
        comps = pt_str.split(':')
        if len(comps) == 1:
            return Position(int(comps[0]), 0)
        elif len(comps) == 2:
            return Position(int(comps[0]), int(comps[1]))
        return None


@total_ordering
class Region(object):
    def __init__(self, start, end=None, view=None, region_key=None):
        self.start = start
        self.end = end or self.start
        self.view = view
        self.region_key = region_key

    def __str__(self):
        return self.to_string()

    def __unicode__(self):
        return self.to_string()

    def to_string(self):
        return u'-'.join([str(self.start), str(self.end)])

    def __eq__(self, other):
        return self.start == other.start and self.end == other.end

    def __lt__(self, other):
        return self.start < other.start or (self.start == other.start and self.end < other.end)

    def from_zero_based(self):
        self.start.from_zero_based()
        self.end.from_zero_based()
        return self

    def to_zero_based(self):
        self.start.to_zero_based()
        self.end.to_zero_based()
        return self

    def to_region(self, view=None):
        if not view:
            view = self.view
        if not view:
            raise RuntimeError('symbols.Region: view is None')
        return sublime.Region(self.start.to_point(view), self.end.to_point(view))

    def update(self):
        if self.saved():
            rgns = self.view.get_regions(self.region_key)
            if rgns:
                rgn = Region.from_region(self.view, rgns[0], self.region_key)
                self.start = rgn.start
                self.end = rgn.end

    def save(self, view, region_key):
        self.view = view
        self.region_key = region_key

    def saved(self):
        return self.view and self.region_key

    def erase(self):
        if self.saved():
            self.view.erase_regions(self.region_key)
            self.view = None
            self.region_key = None

    @staticmethod
    def from_region(view, rgn, region_key=None):
        return Region(Position.from_point(view, rgn.begin()),
                      Position.from_point(view, rgn.end()),
                      view if region_key else None,
                      region_key)

    @staticmethod
    def from_str(rgn_str):
        comps = rgn_str.split('-')
        if len(comps) == 1:
            return Region(Position.from_str(comps[0]))
        elif len(comps) == 2:
            return Region(Position.from_str(comps[0]), Position.from_str(comps[1]))
        return None

    def empty(self):
        return self.start == self.end


class Location(object):
    """
    Location in file at line
    """
    def __init__(self, filename, project=None):
        self.project = project
        self.filename = filename

    def __str__(self):
        return self.to_string()

    def to_string(self):
        return self.filename

    def is_null(self):
        return self.project is None and self.filename is None

    def get_id(self):
        return self.filename

    def __eq__(self, other):
        if isinstance(other, Location):
            return self.filename == other.filename and self.project == other.project
        return False


def source_location(loc, pos):
    """ Returns filename:line:column """
    if not pos:
        return str(loc)
    return ':'.join([str(loc), str(pos)])


class Package(object):
    def __init__(self, name, version=None):
        self.name = name
        self.version = version

    def package_id(self):
        return '{0}-{1}'.format(self.name, self.version) if self.version is not None else self.name

    def __eq__(self, other):
        if isinstance(other, Package):
            return self.name == other.name and self.version == other.version
        return False


def parse_package(package_id):
    if package_id is None:
        return None
    pkg = re.match(r'([\w\-]+)\-([\d\.]+)', package_id)
    if pkg:
        (name, version) = pkg.groups()
        return Package(name, version)
    pkg = re.match(r'([\w\-]+)', package_id)
    if pkg:
        return Package(pkg.groups()[0])
    return None


class PackageDb(object):
    def __init__(self, global_db=False, user_db=False, package_db=None):
        self.global_db = False
        self.user_db = False
        self.package_db = None
        if global_db:
            self.global_db = True
            return
        if user_db:
            self.user_db = True
            return
        if package_db:
            self.package_db = package_db
            return

    def __str__(self):
        return self.to_string()

    def to_string(self):
        if self.global_db:
            return 'global-db'
        if self.user_db:
            return 'user-db'
        if self.package_db:
            return self.package_db

    def __eq__(self, other):
        if isinstance(other, PackageDb):
            return self.global_db == other.global_db and self.user_db == other.user_db and self.package_db == other.package_db
        return False

    @staticmethod
    def from_string(pkgdb_str):
        if pkgdb_str == 'global-db':
            return PackageDb(global_db=True)
        if pkgdb_str == 'user-db':
            return PackageDb(user_db=True)
        return PackageDb(package_db=pkgdb_str)


class InstalledLocation(object):
    """
    Module location in cabal
    """
    def __init__(self, name, package):
        self.name = name
        self.package = package

    def __str__(self):
        return self.to_string()

    def to_string(self):
        return '{0} in {1}'.format(self.name, self.package.package_id())

    def is_null(self):
        return self.package is None

    def get_id(self):
        return '{0}:{1}'.format(self.name, self.package.package_id())

    def __eq__(self, other):
        if isinstance(other, InstalledLocation):
            return self.name == other.name and self.package == other.package
        return False


class OtherLocation(object):
    """
    Other module location
    """
    def __init__(self, source):
        self.source = source

    def __str__(self):
        return self.to_string()

    def to_string(self):
        return self.source or ""

    def is_null(self):
        return self.source is None

    def get_id(self):
        return '[{0}]'.format(self.source)

    def __eq__(self, other):
        if isinstance(other, OtherLocation):
            return self.source == other.source
        return False


def location_package_name(loc):
    if isinstance(loc, InstalledLocation) and loc.package:
        return loc.package.name
    return None


def location_project(loc):
    if isinstance(loc, Location) and loc.project:
        return loc.project
    return None


def location_cabal(loc):
    if isinstance(loc, InstalledLocation):
        return loc.cabal
    return None


def module_location(filename):
    return Location(filename)


def escape_text(txt):
    """
    Escape text, replacing leading spaces to non-breaking ones and newlines to <br> tag
    """
    lines = []
    for line in txt.splitlines():
        lead_spaces = re.match(r'^\s+', line)
        if lead_spaces:  # Replace leading spaces with non-breaking ones
            lines.append(lead_spaces.end() * '&nbsp;' + html.escape(line[lead_spaces.end():], quote=False))
        else:
            lines.append(html.escape(line, quote=False))

    return '<br>'.join(lines)


def unicode_operators(wrap_fn):
    def wrapped(*args, use_unicode=None, **kwargs):
        if use_unicode is False:
            return wrap_fn(*args, **kwargs)
        return UnicodeOpers.use_unicode_operators(wrap_fn(*args, **kwargs), force=(use_unicode is True))
    return wrapped


def wrap_operator(name):
    if re.match(r"[\w']+", name):
        return name
    return "({0})".format(name)


def format_type(expr):
    """
    Format type expression for popup
    Set span 'type' for types and 'tyvar' for type variables
    """
    if not expr:
        return expr
    tyname = re.search(r'([a-zA-Z]\w*)|(->|=>|::|\u2192|\u21d2|\u2237)', expr)
    if tyname:
        tyexpr = expr[tyname.start():tyname.end()]
        expr_class = ''
        if tyname.group(1):
            expr_class = 'type' if tyexpr[0].isupper() else 'tyvar'
        elif tyname.group(2):
            expr_class = 'operator'
        decorated = '<span class="{0}">{1}</span>'.format(expr_class, html.escape(tyexpr, quote=False))
        return html.escape(expr[0:tyname.start()], quote=False) + decorated + format_type(expr[tyname.end():])
    else:
        return html.escape(expr, quote=False)


class ModuleId(object):
    """
    Base for Haskell module
    """
    def __init__(self, name, location):
        self.name = name
        self.location = location

    def __str__(self):
        return u'ModuleId({0} at {1})'.format(self.name, self.location)

    def by_source(self):
        return isinstance(self.location, Location)

    def by_cabal(self):
        return isinstance(self.location, InstalledLocation)

    def by_hayoo(self):
        return isinstance(self.location, OtherLocation)

    def __eq__(self, other):
        if isinstance(other, ModuleId):
            return self.name == other.name and self.location == other.location
        return False


class SymbolId(object):
    """
    Base for Haskell symbols, name with module its defined in
    """
    def __init__(self, name, module):
        self.name = name
        self.module = module

    def __str__(self):
        return u'SymbolId({0} in {1})'.format(self.name, self.module)

    def by_source(self):
        return self.module.by_source()

    def by_cabal(self):
        return self.module.by_cabal()

    def by_hayoo(self):
        return self.module.by_hayoo()

    def __eq__(self, other):
        if isinstance(other, SymbolId):
            return self.name == other.name and self.module == other.module
        return False


class Module(ModuleId):
    """
    Haskell module
    """
    def __init__(self, name, location=None, exports=None, imports=None, last_inspection_time=0):
        super().__init__(name, location)
        self.location = location
        # List of strings
        self.exports = exports[:] if exports is not None else []
        # Dictionary from module name to Import object
        self.imports = imports[:] if imports is not None else []
        for i in self.imports:
            i.location = self.location

        # Time as from time.time()
        self.last_inspection_time = last_inspection_time

    def __str__(self):
        return u'Module({0} at {1} with {2} exports, {3} imports)'.format(
            self.name, self.location, len(self.exports), len(self.imports)
        )

    def __repr__(self):
        return u'Module({0} (exports {1}, imports {2}))'.format(self.name, len(self.exports), len(self.imports))


class Symbol(SymbolId):
    """
    Haskell symbol: function, data, class etc.
    """
    def __init__(self, symbol_type, name, module, docs=None, position=None):
        super().__init__(name, module)
        self.what = symbol_type
        self.docs = docs
        self.position = position

    def __str__(self):
        return u'Symbol({0} {1} in {2})'.format(self.what, self.name, self.module)

    def has_source_location(self):
        return self.by_source() and self.position is not None

    def get_source_location(self):
        if self.has_source_location():
            return source_location(self.defined_module().location, self.position)
        return None

    def defined_module(self):
        return self.module

    def qualified_name(self):
        return '.'.join([self.module.name, self.name])

    def suggest(self):
        """ Returns suggestion for this declaration """
        return ('{0}\t{1}'.format(self.name, self.module.name), self.name)

    def brief(self, _short=False):
        """ Brief information, just a name by default """
        return self.name

    @unicode_operators
    def detailed(self):
        """ Detailed info for use in Symbol Info command """
        parts = [self.brief()]

        if self.docs:
            parts.extend(['', self.docs])

        parts.append('')

        if self.by_source():
            if self.module.location.project:
                parts.append('Project: {0}'.format(self.module.location.project))
        elif self.by_cabal():
            parts.append('Package: {0}'.format(self.module.location.package.package_id()))

        if self.has_source_location():
            parts.append('Defined at: {0}'.format(self.get_source_location()))
        else:
            parts.append('Defined in: {0}'.format(self.module.name))

        return '\n'.join(parts)

    @unicode_operators
    def popup_brief(self):
        """ Brief info on popup with name, possibly with link if have source location """
        info = u'<span class="function">{0}</span>'.format(html.escape(self.name, quote=False))
        if self.has_source_location():
            return u'<a href="{0}">{1}</a>'.format(html.escape(self.get_source_location()), info)

        return info

    @unicode_operators
    def popup(self, comments=None):
        """ Full info on popup with docs """
        parts = [u'<p>']
        parts.append(self.popup_brief())
        for cmnt in comments or []:
            parts.append(u'<br>{0}<span class="comment">-- {1}</span>'.format(4 * '&nbsp;', cmnt))
        # if self.imported_names():
        #     parts.append(u'<br>{0}<span class="comment">-- Imported from {1}</span>'.format(
        #         4 * '&nbsp;',
        #         html.escape(u', '.join(self.imported_names()), quote=False)))
        if self.module:
            module_ref = html.escape(self.module.name, quote=False)
            if self.module.by_source():
                module_ref = u'<a href="{0}">{1}</a>'.format(self.module.location.to_string(), module_ref)
            elif self.module.by_cabal():
                hackage_url = 'http://hackage.haskell.org/package/{0}/docs/{1}.html'.format(
                    self.module.location.package.package_id(),
                    self.module.name.replace('.', '-'))
                module_ref = u'<a href="{0}">{1}</a>'.format(html.escape(hackage_url), module_ref)
            parts.append(u'<br>{0}<span class="comment">-- Defined in {1}</span>'.format(
                4 * '&nbsp;',
                module_ref))
        parts.append(u'</p>')
        if self.docs:
            parts.append(u'<p><span class="docs">{0}</span></p>'.format(escape_text(self.docs)))
        # parts.append(u'<a href="info">...</a>')
        return u''.join(parts)


class Import(object):
    """
    Haskell import of module
    """
    def __init__(self, module_name, is_qualified=False, import_as=None, position=None, location=None):
        self.module = module_name
        self.is_qualified = is_qualified
        self.import_as = import_as
        self.position = position
        self.location = location

    def __repr__(self):
        return u'Import({0}{1}{2})'.format(self.module, ' qualified' if self.is_qualified else '',
                                           ' as ' + self.import_as if self.import_as else '')

    def dump(self):
        return self.__dict__


class Function(Symbol):
    """
    Haskell function declaration
    """
    def __init__(self, name, module, function_type, docs=None, position=None):
        super().__init__('function', name, module, docs=docs, position=position)
        self.type = function_type

    def __repr__(self):
        return u'Function({0} :: {1} [{2}])'.format(wrap_operator(self.name), self.type, self.module.name)

    def suggest(self):
        return (UnicodeOpers.use_unicode_operators(u'{0} :: {1}\t{2}'.format(wrap_operator(self.name),
                                                                             self.type,
                                                                             self.module.name)),
                self.name)

    @unicode_operators
    def brief(self, short=False):
        if short:
            return u'{0}'.format(wrap_operator(self.name))
        return u'{0} :: {1}'.format(wrap_operator(self.name), self.type if self.type else u'?')

    @unicode_operators
    def popup_brief(self):
        info = u'<span class="function">{0}</span>'.format(html.escape(self.name, quote=False))
        if self.has_source_location():
            info = u'<a href="{0}">{1}</a>'.format(html.escape(self.get_source_location()), info)
        return u'{0} <span class="operator">::</span> {1}'.format(info, format_type(self.type if self.type else u'?'))


class TypeBase(Symbol):
    """
    Haskell type, data or class
    """
    def __init__(self, decl_type, name, module, context, args, docs=None, position=None):
        super().__init__(decl_type, name, module, docs=docs, position=position)
        self.context = context
        self.args = args

    def suggest(self):
        return (UnicodeOpers.use_unicode_operators(u'{0} {1}\t{2}'.format(self.name,
                                                                          ' '.join(self.args),
                                                                          self.module.name)),
                self.name)

    @unicode_operators
    def brief(self, short=False):
        if short:
            brief_parts = [self.what, self.name]
            if self.args:
                brief_parts.extend(self.args)
            return u' '.join(brief_parts)

        brief_parts = [self.what]
        if self.context:
            if len(self.context) == 1:
                brief_parts.append(u'{0} =>'.format(self.context[0]))
            else:
                brief_parts.append(u'({0}) =>'.format(', '.join(self.context)))

        brief_parts.append(self.name)
        if self.args:
            brief_parts.append(u' '.join(self.args))

        return u' '.join(brief_parts)

    @unicode_operators
    def popup_brief(self):
        parts = [u'<span class="keyword">{0}</span>'.format(html.escape(self.what, quote=False))]
        if self.context:
            if len(self.context) == 1:
                parts.append(format_type(u'{0} =>'.format(self.context[0])))
            else:
                parts.append(format_type(u'({0}) =>'.format(', '.join(self.context))))

        name_part = u'<span class="type">{0}</span>'.format(html.escape(self.name, quote=False))
        if self.has_source_location():
            name_part = u'<a href="{0}">{1}</a>'.format(html.escape(self.get_source_location()), name_part)
        parts.append(name_part)

        if self.args:
            parts.append(format_type(u' '.join(self.args)))

        return u' '.join(parts)


class Type(TypeBase):
    """
    Haskell type synonym
    """
    def __init__(self, name, module, context, args, docs=None, position=None):
        super().__init__('type', name, module, context, args, docs=docs, position=position)


class Newtype(TypeBase):
    """
    Haskell newtype synonym
    """
    def __init__(self, name, module, context, args, docs=None, position=None):
        super().__init__('newtype', name, module, context, args, docs=docs, position=position)


class Data(TypeBase):
    """
    Haskell data declaration
    """
    def __init__(self, name, module, context, args, docs=None, position=None):
        super().__init__('data', name, module, context, args, docs=docs, position=position)


class Class(TypeBase):
    """
    Haskell class declaration
    """
    def __init__(self, name, module, context, args, docs=None, position=None):
        super().__init__('class', name, module, context, args, docs=docs, position=position)

    def __repr__(self):
        return u'Class({0})'.format(self.name)


class UnknownSymbol(Symbol):
    def __init__(self, symbol_type, name, module, docs=None, position=None, **kwargs):
        super().__init__(symbol_type, name, module, docs=docs, position=position)
        for field in ["type", "parent_class", "parent", "constructors", "args", "ctx", "associate", "pat_type", "constructor"]:
            kwargs.setdefault(field, None)
        self.__dict__.update(kwargs)

    def suggest(self):
        return (
            UnicodeOpers.use_unicode_operators(
                u'{0}\t{1}'.format(
                    wrap_operator(self.name),
                    self.module.name
                )
            ),
            self.name
        )

    @unicode_operators
    def brief(self, short=False):
        if short:
            return u'{0}'.format(wrap_operator(self.name))
        return u'{0} :: {1}'.format(wrap_operator(self.name), self.type if self.type else u'?')

    @unicode_operators
    def popup_brief(self):
        info = u'<span class="function">{0}</span>'.format(html.escape(self.name, quote=False))
        if self.has_source_location():
            info = u'<a href="{0}">{1}</a>'.format(html.escape(self.get_source_location()), info)
        return u'{0} <span class="operator">::</span> {1}'.format(info, format_type(self.type if self.type else u'?'))


def update_with(left, right, default_value, upd_fn):
    """
    unionWith for Python, but modifying first dictionary instead of returning result
    """
    for k, val in right.items():
        if k not in left:
            left[k] = default_value[:]
        left[k] = upd_fn(left[k], val)
    return left


def same_module(left, right):
    """
    Returns true if l is same module as r, which is when module name is equal
    and modules defined in one file, in same cabal-dev sandbox or in cabal
    """
    same_cabal = left.cabal and right.cabal and (left.cabal == right.cabal)
    same_filename = left.location and right.location and (left.location.filename == right.location.filename)
    nowhere = (not left.cabal) and (not left.location) and (not right.cabal) and (not right.location)
    return left.name == right.name and (same_cabal or same_filename or nowhere)


def same_declaration(left, right):
    """
    Returns true if l is same declaration as r
    """
    same_mod = left.module and right.module and same_module(left.module, right.module)
    nowhere = (not left.module) and (not right.module)
    return left.name == right.name and (same_mod or nowhere)


def is_within_project(module, project):
    """
    Returns whether module defined within project specified
    """
    if module.location:
        return module.location.project == project
    return False


def is_within_cabal(module, cabal=None):
    """
    Returns whether module loaded from cabal specified
    """
    return cabal is not None and module.cabal == cabal


def is_by_sources(module):
    """
    Returns whether module defined by sources
    """
    return module.location is not None


def flatten(lsts):
    return reduce(lambda l, r: list(l) + list(r), lsts)


class CabalPackage(object):
    def __init__(self, name, synopsis, version, installed, homepage, pkg_license):
        self.name = name
        self.synopsis = synopsis
        self.default_version = version
        self.installed_versions = installed[:] if installed else []
        self.homepage = homepage
        self.license = pkg_license

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


class SymbolUsage(object):
    def __init__(self, symbol, used_in, position):
        self.symbol = symbol
        self.used_in = used_in
        self.position = position

    def __str__(self):
        return u'{0}: {1}'.format(
            source_location(self.used_in.location, self.position),
            self.symbol.name
        )


class Corrector(object):
    def __init__(self, region, contents):
        self.region = region
        self.contents = contents

    # def __eq__(self, other):
    #     return self.region == other.region and self.contents == other.contents

    def to_region(self, view):
        return self.region.to_region(view)

    def from_region(self, view, rgn):
        self.region = Region.from_region(view, rgn)

    def to_json(self):
        return json.dumps({self.region.to_string():self.contents})

    @staticmethod
    def from_json(j):
        corrs = json.loads(j)
        its = list(corrs.items())
        if len(its) == 1:
            (rgn, cts) = its[0]
            return Corrector(Region.from_str(rgn), cts)
        return None


class Correction(object):
    def __init__(self, file, level, message, corrector, message_region=None):
        self.file = file
        self.level = level or 'hint'
        self.message = message
        # source messages region, used to match corrector and outputmessage
        self.message_region = message_region
        self.corrector = corrector

    def to_region(self, view):
        return self.corrector.to_region(view)

    def from_region(self, view, rgn):
        self.corrector.from_region(view, rgn)

    @unicode_operators
    def detailed(self):
        if self.corrector.contents:
            return u'\u2014 {0}\n  Why not:\n\n{1}'.format(self.message, self.corrector.contents)
        return u'\u2014 {0}'.format(self.message)

    @unicode_operators
    def popup(self):
        arg1 = html.escape(self.message)
        arg2 = html.escape("autofix:" + self.corrector.region.to_string())
        parts = [u'<span class="message">— {0} (<a href="{1}">{2}</a>)</span>'.format(arg1, arg2, "autofix")]
        if self.corrector.contents:
            parts.append(u'<br>{0}<span class="{1}">{2}</span>'.format(2 * "&nbsp;",
                                                                       html.escape(self.level),
                                                                       html.escape("Why not: ")))
            parts.append(u'<br><br>{0}<span class="code">{1}</span>'.format(4 * "&nbsp;", html.escape(self.corrector.contents)))
        return u''.join(parts)



def mark_corrections(views, corrs):
    for view in views:
        if view.file_name() is None:
            continue
        corrs_ = [corr for corr in corrs if os.path.samefile(corr.file, view.file_name())]
        view.add_regions('autofix', [corr.to_region(view) for corr in corrs_], 'entity.name.function', 'dot', 0)


# This allow us track region positions even if file modifies
def add_corrections_regions(views, corrs):
    for view in views:
        if view.file_name() is None:
            continue
        corrs_ = [corr for corr in corrs if os.path.samefile(corr.file, view.file_name())]
        view.add_regions('autofix', [corr.to_region(view) for corr in corrs_], 'autofix', '', sublime.HIDDEN)


# Restore actual region data from view regions
def restore_corrections_regions(views, corrs):
    for view in views:
        if view.file_name() is None:
            continue
        corrs_ = [corr for corr in corrs if os.path.samefile(corr.file, view.file_name())]
        rgns = view.get_regions('autofix')
        if len(rgns) != len(corrs_):
            # Something wrong
            return

        for rgn, corr in zip(rgns, corrs):
            corr.corrector.from_region(view, rgn)
