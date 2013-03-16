import os
import re
import sublime
import sublime_plugin
import subprocess
import threading

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    from ghci import ghci_package_db, parse_info
    import symbols
else:
    from SublimeHaskell.sublime_haskell_common import *
    from SublimeHaskell.ghci import ghci_package_db, parse_info
    import SublimeHaskell.symbols as symbols

def call_hdevtools_and_wait(arg_list, filename = None, cabal = None):
    """
    Calls hdevtools with the given arguments.
    Shows a sublime error message if hdevtools is not available.
    """

    ghc_opts = get_setting_async('ghc_opts')
    hdevtools_socket = get_setting_async('hdevtools_socket')

    package_db = ghci_package_db()
    if package_db:
        ghs_opts.append('-package_db {0}'.format(package_db))

    source_dir = get_source_dir(filename)
    ghc_opts.append('-i {0}'.format(source_dir))

    ghc_opts_args = []
    if ghc_opts:
        for opt in ghc_opts:
            ghc_opts_args.extend(["-g", opt])

    if hdevtools_socket:
        arg_list.append('--socket={0}'.format(hdevtools_socket))

    try:
        command = ['hdevtools'] + arg_list + ghc_opts_args

        exit_code, out, err = call_and_wait(['hdevtools'] + arg_list + ghc_opts_args, cwd = source_dir)

        if exit_code != 0:
            raise Exception("hdevtools exited with status %d and stderr: %s" % (exit_code, err))

        return crlf2lf(out)

    except OSError as e:
        if e.errno == errno.ENOENT:
            output_error(sublime.active_window(),
                "SublimeHaskell: hdevtools was not found!\n"
                + "It's used for 'symbol info' and type inference\n"
                + "Try adjusting the 'add_to_PATH' setting.\n"
                + "'enable_hdevtools' automatically set to False.")

        set_setting_async('enable_hdevtools', False)

        return None

def admin(cmds, wait = False, **popen_kwargs):
    hdevtools_socket = get_setting_async('hdevtools_socket')

    if hdevtools_socket:
        cmds.append('--socket={0}'.format(hdevtools_socket))

    command = ["hdevtools", "admin"] + cmds

    if wait:
        (exit_code, stdout, stderr) = call_and_wait(command, **popen_kwargs)
        if exit_code == 0:
            return stdout
        return ''
    else:
        call_no_wait(command, **popen_kwargs)
        return ''

def is_running():
    r = admin(['--status'], wait = True)
    if re.search(r'running', r):
        return True
    else:
        return False

def start_server():
    if not is_running():
        admin(["--start-server"])

def hdevtools_info(filename, symbol_name, cabal = None):
    """
    Uses hdevtools info filename symbol_name to get symbol info
    """
    contents = call_hdevtools_and_wait(['info', filename, symbol_name], filename = filename, cabal = cabal)
    return parse_info(symbol_name, contents)

def hdevtools_check(filename, cabal = None):
    """
    Uses hdevtools to check file
    """
    contents = call_hdevtools_and_wait(['check', filename], filename = filename, cabal = cabal)
    return contents

def hdevtools_type(filename, line, column, cabal = None):
    """
    Uses hdevtools to infer type
    """
    return call_hdevtools_and_wait(['type', filename, str(line), str(column)], filename = filename, cabal = cabal)

def plugin_loaded():
    thread = threading.Thread(
        target=start_server)
    thread.start()

def plugin_unloaded():
    admin(["--stop-server"])

if int(sublime.version()) < 3000:
    plugin_loaded()
