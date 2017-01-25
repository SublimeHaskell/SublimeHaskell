# -*- coding: UTF-8 -*-

import re
import sublime
import threading

if int(sublime.version()) < 3000:
    from sublime_haskell_common import *
    from parseoutput import parse_info
else:
    from SublimeHaskell.sublime_haskell_common import *
    from SublimeHaskell.parseoutput import parse_info


class DescriptorDrain(threading.Thread):
    """Continually running thread that drains a Python file, sending everything read to stdout (which in ST's case
    is a logging object)"""

    ### This really belongs in sublime_haskell_common. But, since that module gets loaded later than this one OR
    ### it gets reloaded, you end up with the dreaded super() TypeError.
    def __init__(self, label, fd):
        super(DescriptorDrain, self).__init__(name = 'drain-' + label)
        self.label = label
        self.fd = fd
        self.stop_me = threading.Event()

    def run(self):
        while not self.stop_me.is_set():
            l = crlf2lf(decode_bytes(self.fd.readline())).rstrip()
            print('<{0}> {1}'.format(self.label, l))

    def stop(self):
        self.stop_me.set()


def show_hdevtools_error_and_disable():
    # Looks like we can't always get an active window here,
    # we use sublime.error_message() instead of
    # output_error(sublime.active_window().
    sublime.set_timeout(lambda: sublime.error_message(
        "SublimeHaskell: hdevtools was not found!\n"
        "It's used for 'symbol info' and type inference\n"
        "Install it with 'cabal install hdevtools',\n"
        "or adjust the 'add_to_PATH' setting for a custom location.\n"
        "'enable_hdevtools' automatically set to False in the User settings."), 0)

    set_setting_async('enable_hdevtools', False)


def call_hdevtools_and_wait(arg_list, filename = None, cabal = None):
    """
    Calls hdevtools with the given arguments.
    Shows a sublime error message if hdevtools is not available.
    """
    ghc_opts_args = get_ghc_opts_args(filename, cabal = cabal)
    hdevtools_socket = get_setting_async('hdevtools_socket')
    source_dir = get_source_dir(filename)

    if hdevtools_socket:
        arg_list.append('--socket={0}'.format(hdevtools_socket))

    try:
        exit_code, out, err = ProcHelper.run_process(['hdevtools'] + arg_list + ghc_opts_args, cwd = source_dir)
        if exit_code != 0:
            show_hdevtools_error_and_disable()
            raise Exception("hdevtools exited with status %d and stderr: %s" % (exit_code, err))

        return crlf2lf(out)

    except OSError as e:
        if e.errno == errno.ENOENT:
            show_hdevtools_error_and_disable()

        return None

    except Exception as e:
        log('calling to hdevtools fails with {0}'.format(e), log_error)
        return None


def admin(cmds, wait = False, **popen_kwargs):
    if not get_setting_async('enable_hdevtools'):
        return None

    hdevtools_socket = get_setting_async('hdevtools_socket')

    if hdevtools_socket:
        cmds.append('--socket={0}'.format(hdevtools_socket))

    command = ["hdevtools", "admin"] + cmds

    try:
        if wait:
            exit_code, stdout, stderr = ProcHelper.run_process(command, **popen_kwargs)
            return stdout if exit_code == 0 else 'error running {0}: {1}'.format(command, stderr)
        else:
            p = ProcHelper(command, '', **popen_kwargs)
            DescriptorDrain('hdevtools stdout', p.stdout).start()
            DescriptorDrain('hdevtools stderr', p.stderr).start()
            return ''

    except OSError as e:
        if e.errno == errno.ENOENT:
            show_hdevtools_error_and_disable()

        set_setting_async('enable_hdevtools', False)

        return None
    except Exception as e:
        log('calling to hdevtools fails with {0}'.format(e))
        return None


def is_running():
    r = admin(['--status'], wait = True)
    if r and re.search(r'running', r):
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
    return parse_info(symbol_name, contents) if contents else None


def hdevtools_check(filename, cabal = None):
    """
    Uses hdevtools to check file
    """
    return call_hdevtools_and_wait(['check', filename], filename = filename, cabal = cabal)


def hdevtools_type(filename, line, column, cabal = None):
    """
    Uses hdevtools to infer type
    """
    return call_hdevtools_and_wait(['type', filename, str(line), str(column)], filename = filename, cabal = cabal)


def start_hdevtools():
    thread = threading.Thread(target=start_server)
    thread.start()


def stop_hdevtools():
    admin(["--stop-server"])
