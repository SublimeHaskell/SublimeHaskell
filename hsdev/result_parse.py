
import SublimeHaskell.symbols as symbols


def parse_list(fn, xs):
    return [fn(x) for x in xs] if xs is not None else []


def parse_modules(modules):
    return parse_list(parse_module, modules)


def parse_symbols(symbols):
    return parse_list(parse_symbol, symbols)


def parse_module_ids(module_ids):
    return parse_list(parse_module_id, module_ids)


def parse_symbol_ids(symbol_ids):
    return parse_list(parse_symbol_id, symbol_ids)


def parse_module_id(mod):
    if not mod:
        return None
    return symbols.ModuleId(
        mod['name'],
        parse_location(mod.get('location')),
    )


def parse_symbol_id(sym):
    if not sym:
        return None
    return symbols.SymbolId(
        sym['name'],
        parse_module_id(sym['module']),
    )


def parse_module(mod):
    if not mod:
        return None
    mid = parse_module_id(mod['id'])
    return symbols.Module(
        mid.name,
        mid.location,
        exports=parse_symbols(mod.get('exports')),
    )


def parse_symbol(sym):
    if not sym:
        return None

    sid = parse_symbol_id(sym['id'])
    docs = sym.get('docs')
    pos = parse_position(sym.get('pos'))

    sinfo = sym['info']
    what = sinfo['what']

    type_symbols = {
        'type': symbols.Type,
        'newtype': symbols.Newtype,
        'data': symbols.Data,
        'class': symbols.Class,
    }

    if what == 'function':
        return symbols.Function(
            sid.name,
            sid.module,
            function_type=sinfo.get('type'),
            docs=docs,
            position=pos,
        )
    elif what in type_symbols:
        ctx = sinfo.get('ctx')
        args = sinfo.get('args')

        return type_symbols[what](
            sid.name,
            sid.module,
            context=ctx,
            args=args,
            docs=docs,
            position=pos,
        )
    else:
        sinfo.pop('what')
        fields = dict((name.replace('-', '_').replace('class', 'parent_class'), value) for name, value in sinfo.items())
        return symbols.UnknownSymbol(
            what,
            sid.name,
            sid.module,
            **fields
        )


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
    if srclocation is None:
        return None
    if 'file' in srclocation:
        return symbols.Location(
            get_value(srclocation, 'file'),
            get_value(srclocation, 'project'),
        )
    elif 'package' in srclocation:
        return symbols.InstalledLocation(
            get_value(srclocation, 'name'),
            symbols.parse_package(get_value(srclocation, 'package')),
        )
    elif 'source' in srclocation:
        return symbols.OtherLocation(get_value(srclocation, 'source'))
    else:
        raise RuntimeError("Can't parse location: {0}".format(srclocation))


def parse_import(imp):
    return symbols.Import(imp['name'], imp['qualified'], imp.get('as'), parse_position(imp.get('pos'))) if imp else None


def parse_cabal_package(pkg):
    retval = None
    if pkg is not None:
        retval = symbols.CabalPackage(pkg['name'], pkg.get('synopsis'), pkg.get('default-version'),
                                      pkg.get('installed-versions'), pkg.get('homepage'), pkg.get('license'))
    return retval


def parse_corrections(corr):
    return [parse_correction(c) for c in corr] if corr else None


def parse_correction(corr):
    return symbols.Correction(
        corr['source']['file'],
        corr['level'],
        corr['note']['message'],
        parse_corrector(corr['note']['action']),
        parse_region(corr.get('region')),
    )


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
