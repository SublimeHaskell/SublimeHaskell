
import errno
import os
import re

import sublime

if int(sublime.version()) < 3000:
    import sublime_haskell_common as Common
    import internals.settings as Settings
    import internals.proc_helper as ProcHelper
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.settings as Settings
    import SublimeHaskell.internals.proc_helper as ProcHelper

def ghci_package_db(cabal=None):
    if not cabal or cabal == 'cabal':
        return None
    package_conf = (filter(lambda x: re.match(r'packages-(.*)\.conf', x), os.listdir(cabal)) + [None])[0]
    if package_conf:
        return os.path.join(cabal, package_conf)
    return None


def get_ghc_opts(filename=None, add_package_db=True, cabal=None):
    """
    Gets ghc_opts, used in several tools, as list with extra '-package-db' option and '-i' option if filename passed
    """
    ghc_opts = Settings.get_setting_async('ghc_opts')
    if not ghc_opts:
        ghc_opts = []
    if add_package_db:
        package_db = ghci_package_db(cabal=cabal)
        if package_db:
            ghc_opts.append('-package-db {0}'.format(package_db))

    if filename:
        ghc_opts.append('-i {0}'.format(ProcHelper.get_source_dir(filename)))

    return ghc_opts


def get_ghc_opts_args(filename=None, add_package_db=True, cabal=None):
    """
    Same as ghc_opts, but uses '-g' option for each option
    """
    opts = get_ghc_opts(filename, add_package_db, cabal)
    args = []
    for opt in opts:
        args.extend(["-g", "\"" + opt + "\""])
    return args


def call_ghcmod_and_wait(arg_list, filename=None, cabal=None):
    """
    Calls ghc-mod with the given arguments.
    Shows a sublime error message if ghc-mod is not available.
    """

    ghc_opts_args = get_ghc_opts_args(filename, add_package_db=False, cabal=cabal)

    try:
        command = ['ghc-mod'] + ghc_opts_args + arg_list

        # Logging.log('running ghc-mod: {0}'.format(command))

        # Set cwd to user directory
        # Otherwise ghc-mod will fail with 'cannot satisfy package...'
        # Seems, that user directory works well
        # Current source directory is set with -i argument in get_ghc_opts_args
        #
        # When cabal project is available current directory is set to the project root
        # to avoid troubles with possible template haskell openFile calls
        ghc_mod_current_dir = ProcHelper.get_source_dir(filename)
        if filename:
            cabal_project_dir = Common.get_cabal_project_dir_of_file(filename)
            if cabal_project_dir:
                ghc_mod_current_dir = cabal_project_dir
        exit_code, out, err = ProcHelper.ProcHelper.run_process(command, cwd=ghc_mod_current_dir)

        if exit_code != 0:
            raise Exception("%s exited with status %d and stderr: %s" % (' '.join(command), exit_code, err))

        # return crlf2lf(out)
        return out

    except OSError as os_exc:
        if os_exc.errno == errno.ENOENT:
            Common.output_error_async(sublime.active_window(),
                                      "SublimeHaskell: ghc-mod was not found!\n"
                                      "It is used for LANGUAGE and import autocompletions and type inference.\n"
                                      "Try adjusting the 'add_to_PATH' setting.\n"
                                      "You can also turn this off using the 'enable_ghc_mod' setting.")
        # Re-raise so that calling code doesn't try to work on the `None` return value
        raise os_exc


def ghcmod_type(filename, module_name, line, column, cabal = None):
    """
    Uses ghc-mod type to infer type
    """
    return call_ghcmod_and_wait(['type', filename, module_name, str(line), str(column)], filename = filename, cabal = cabal)


def ghcmod_info(filename, module_name, symbol_name, cabal = None):
    """
    Uses ghc-mod info filename module_name symbol_name to get symbol info
    """
    contents = GHCIMod.call_ghcmod_and_wait(['info', filename, module_name, symbol_name], filename = filename, cabal = cabal)
    # TODO: Returned symbol doesn't contain location
    # But in fact we use ghcmod_info only to retrieve type of symbol
    return parse_info(symbol_name, contents)
