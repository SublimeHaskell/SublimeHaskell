
import SublimeHaskell.symbols as symbols

def parse_decls(decls):
    return [parse_module_declaration(decl) for decl in decls] if decls is not None else []


def parse_modules_brief(mods):
    return [parse_module_id(m) for m in mods] if mods is not None else []


def get_value(table, keyvals, defval=None):
    if table is None:
        return defval
    if isinstance(keyvals, list):
        cur = table
        for k in keyvals:
            cur = cur.get(k)
            if cur is None:
                return defval
        return cur

    return table.get(keyvals, defval)


# 'global-db', 'user-db' or {'package-db':path}
def parse_package_db(pkgdb, defval=None):
    if isinstance(pkgdb, dict):
        pdb = get_value(pkgdb, 'package-db')
        return symbols.PackageDb(package_db=pdb) if pdb else defval
    if pkgdb == 'global-db':
        return symbols.PackageDb(global_db=True)
    if pkgdb == 'user-db':
        return symbols.PackageDb(user_db=True)

    return defval


def parse_position(posn):
    if not posn:
        return None
    line = get_value(posn, 'line')
    column = get_value(posn, 'column')
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


def parse_location(srclocation):
    loc = symbols.Location(get_value(srclocation, 'file'), get_value(srclocation, 'project'))
    if loc.is_null():
        loc = symbols.InstalledLocation(symbols.parse_package(get_value(srclocation, 'package')),
                                        parse_package_db(get_value(srclocation, 'db')))
        if loc.is_null():
            loc = symbols.OtherLocation(get_value(srclocation, 'source'))

    return loc if not loc.is_null() else None


def parse_import(imp):
    return symbols.Import(imp['name'], imp['qualified'], imp.get('as'), parse_position(imp.get('pos'))) if imp else None


def parse_module_id(mod):
    return symbols.Module(mod['name'], [], [], {}, parse_location(mod.get('location'))) if mod else None


def parse_declaration(decl):
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

    retval = None
    if what == 'function':
        ## Most common path
        retval = symbols.Function(name, the_decl.get('type'), docs, imported, defined, pos)
    else:
        decl_ctx = decl_info.get('ctx')
        decl_args = decl_info.get('args', [])
        decl_def = decl_info.get('def')

        if what == 'type':
            retval = symbols.Type(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)
        elif what == 'newtype':
            retval = symbols.Newtype(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)
        elif what == 'data':
            retval = symbols.Data(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)
        elif what == 'class':
            retval = symbols.Class(name, decl_ctx, decl_args, decl_def, docs, imported, defined, pos)

    return retval

def parse_declarations(decls):
    return list(map(parse_declaration, decls)) if decls is not None else []


def parse_module_declaration(mod_decl, parse_module_info=True):
    try:
        mod_id = None
        if 'module-id' in mod_decl and parse_module_info:
            mod_id = parse_module_id(mod_decl['module-id'])

        # loc = parse_location(d['module-id'].get('location'))
        decl = parse_declaration(mod_decl['declaration'])
        if decl:
            decl.module = mod_id
            return decl

        return None

    except AttributeError:
        return None


def parse_module(mod):
    if mod is None:
        return None

    the_imports = [parse_import(i) for i in mod['imports']] if 'imports' in mod else []
    the_decls = dict((decl['name'], parse_declaration(decl)) for decl in mod['declarations']) if 'declarations' in mod else {}
    return symbols.Module(mod['name'], mod.get('exports'), the_imports, the_decls, parse_location(mod.get('location')))


def parse_modules(modules):
    if modules is None:
        return None
    return [parse_module(mod) for mod in modules]


def parse_cabal_package(pkg):
    retval = None
    if pkg is not None:
        retval = symbols.CabalPackage(pkg['name'], pkg.get('synopsis'), pkg.get('default-version'),
                                      pkg.get('installed-versions'), pkg.get('homepage'), pkg.get('license'))
    return retval


def parse_corrections(corr):
    return [parse_correction(c) for c in corr] if corr else None

def parse_correction(corr):
    return symbols.Correction(corr['source']['file']
                              , corr['level']
                              , corr['note']['message']
                              , parse_corrector(corr['note']['corrector'])
                              , parse_region(corr.get('region')))


def parse_corrector(corr):
    return symbols.Corrector(parse_region(corr['region']), corr['contents'])


def encode_corrections(corrections):
    return list(map(encode_correction, corrections))


def encode_correction(corr):
    return {
        'source': {
            'project': None,
            'file': corr.file},
        'level': corr.level,
        'note': {
            'corrector': encode_corrector(corr.corrector),
            'message': corr.message},
        'region': {
            'from': encode_position(corr.corrector.region.start.from_zero_based()),
            'to': encode_position(corr.corrector.region.end.from_zero_based())}
        }


def encode_corrector(corr):
    return {
        'region': {
            'from': encode_position(corr.region.start),
            'to': encode_position(corr.region.end)},
        'contents': corr.contents}


def encode_position(posn):
    return {
        'line': posn.line,
        'column': posn.column}


def encode_package_db(pkgdb):
    if pkgdb.user_db:
        return 'user-db'
    if pkgdb.global_db:
        return 'global-db'
    if pkgdb.package_db:
        return {'package-db': pkgdb.package_db}
    return None
