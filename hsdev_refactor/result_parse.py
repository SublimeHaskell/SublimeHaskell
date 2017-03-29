
import traceback

import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.symbols as symbols

def parse_decls(decls):
    return [parse_module_declaration(decl) for decl in decls] if decls is not None else []


def parse_modules_brief(mods):
    return [parse_module_id(m) for m in mods] if mods is not None else []


def get_value(dc, ks, defval=None):
    if dc is None:
        return defval
    if isinstance(ks, list):
        cur = dc
        for k in ks:
            cur = cur.get(k)
            if cur is None:
                return defval
        return cur
    else:
        return dc.get(ks, defval)


# 'global-db', 'user-db' or {'package-db':path}
def parse_package_db(d, defval=None):
    if type(d) == dict:
        pdb = get_value(d, 'package-db')
        return symbols.PackageDb(package_db=pdb) if pdb else defval
    if d == 'global-db':
        return symbols.PackageDb(global_db=True)
    if d == 'user-db':
        return symbols.PackageDb(user_db=True)
    return defval


def parse_position(d):
    if not d:
        return None
    line = get_value(d, 'line')
    column = get_value(d, 'column')
    if line is not None and column is not None:
        return symbols.Position(line, column)
    return None


def parse_region(rgn):
    if rgn is not None:
        start = parse_position(rgn.get('from'))
        end = parse_position(rgn.get('to'))
        if start is not None and end is not None:
            return symbols.Region(start, end)

    return None


def parse_location(d):
    loc = symbols.Location(
        get_value(d, 'file'),
        get_value(d, 'project'))
    if not loc.is_null():
        return loc
    loc = symbols.InstalledLocation(
        symbols.parse_package(get_value(d, 'package')),
        parse_package_db(get_value(d, 'db')))
    if not loc.is_null():
        return loc
    loc = symbols.OtherLocation(
        get_value(d, 'source'))
    if not loc.is_null():
        return loc
    return None


def parse_import(d):
    if not d:
        return None
    return symbols.Import(d['name'], d['qualified'], d.get('as'), parse_position(d.get('pos')))


def parse_module_id(d):
    if d is None:
        return None
    return symbols.Module(
        d['name'],
        [], [], {},
        parse_location(d.get('location')))


def parse_declaration(decl):
    try:
        what = decl['decl']['what']
        docs = decl.get('docs')
        name = decl['name']
        pos = parse_position(decl.get('pos'))
        imported = []
        if 'imported' in decl and decl['imported']:
            imported = [parse_import(d) for d in decl['imported']]
        defined = None
        if 'defined' in decl and decl['defined']:
            defined = parse_module_id(decl['defined'])

        the_decl = decl['decl']
        decl_info = the_decl.get('info')

        if what == 'function':
            return symbols.Function(name, the_decl.get('type'), docs, imported, defined, pos)
        else:
            decl_ctx = decl_info.get('ctx')
            decl_args = decl_info.get('args', [])
            decl_def = decl_info.get('def')

            if what == 'type':
                return symbols.Type(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)
            elif what == 'newtype':
                return symbols.Newtype(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)
            elif what == 'data':
                return symbols.Data(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)
            elif what == 'class':
                return symbols.Class(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)
            else:
                return None
    except:
        Logging.log('Error pasring declaration, see console window traceback', Logging.LOG_ERROR)
        print(traceback.format_exc())
        return None


def parse_declarations(decls):
    return list(map(parse_declaration, decls)) if decls is not None else []


def parse_module_declaration(d, parse_module_info=True):
    try:
        m = None
        if 'module-id' in d and parse_module_info:
            m = parse_module_id(d['module-id'])

        # loc = parse_location(d['module-id'].get('location'))
        decl = parse_declaration(d['declaration'])
        if decl:
            decl.module = m
            return decl
        else:
            return None
    except:
        return None


def parse_module(d):
    if d is None:
        return None
    return symbols.Module(
        d['name'],
        d.get('exports'),
        [parse_import(i) for i in d['imports']] if 'imports' in d else [],
        dict((decl['name'], parse_declaration(decl)) for decl in d['declarations']) if 'declarations' in d else {},
        parse_location(d.get('location')))


def parse_modules(ds):
    if ds is None:
        return None
    return [parse_module(d) for d in ds]


def parse_cabal_package(d):
    if d is None:
        return None
    return symbols.CabalPackage(d['name'], d.get('synopsis'), d.get('default-version'), d.get('installed-versions'),
                                d.get('homepage'), d.get('license'))


def parse_corrections(d):
    return list(map(parse_correction, d)) if d is not None else d


def parse_correction(d):
    return symbols.Correction(d['source']['file']
                              , d['level']
                              , d['note']['message']
                              , parse_corrector(d['note']['corrector'])
                              , parse_region(d.get('region')))


def parse_corrector(d):
    return symbols.Corrector(parse_region(d['region']), d['contents'])


def encode_corrections(corrections):
    return list(map(encode_correction, corrections))


def encode_correction(c):
    return {
        'source': {
            'project': None,
            'file': c.file},
        'level': c.level,
        'note': {
            'corrector': encode_corrector(c.corrector),
            'message': c.message},
        'region': {
            'from': encode_position(c.corrector.start.from_zero_based()),
            'to': encode_position(c.corrector.end.from_zero_based())}
        }


def encode_corrector(c):
    return {
        'region': {
            'from': encode_position(c.start),
            'to': encode_position(c.end)},
        'contents': c.contents}


def encode_position(p):
    return {
        'line': p.line,
        'column': p.column}


def encode_package_db(db):
    if db.user_db:
        return 'user-db'
    if db.global_db:
        return 'global-db'
    if db.package_db:
        return {'package-db': db.package_db}
    return None
