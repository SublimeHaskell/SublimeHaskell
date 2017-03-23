# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# ProcHelper: Process execution helper class.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import errno
import io
import json
import subprocess
import os
import os.path
import re
import platform

import sublime

if int(sublime.version()) < 3000:
    # from internals.locked_object import LockedObject
    # from internals.settings import get_setting_async
    # from internal.utils import decode_bytes, encode_bytes
    pass
else:
    import SublimeHaskell.sublime_haskell_common as Common
    import SublimeHaskell.internals.logging as Logging
    import SublimeHaskell.internals.locked_object as LockedObject
    import SublimeHaskell.internals.settings as Settings
    import SublimeHaskell.internals.utils as Utils

def isWinXX():
    return platform.system() == "Windows"

class ProcHelper(object):
    """Command and tool process execution helper."""

    # Tool name -> executable path cache. Avoids probing the file system multiple times.
    which_cache = LockedObject.LockedObject({})

    # Augmented environment for the subprocesses. Specifically, we really want
    # to augment the user's PATH used to search for executables and tools:
    augmented_env = None

    def __init__(self, command, input_string='', **popen_kwargs):
        """Open a pipe to a command or tool."""

        if ProcHelper.augmented_env is None:
            ProcHelper.augmented_env = ProcHelper.get_extended_env()

        self.process = None
        self.process_err = None

        if isWinXX():
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            popen_kwargs['startupinfo'] = startupinfo

        # Allow caller to specify something different for stdout or stderr -- provide
        # the default here if unspecified.
        if popen_kwargs.get('stdout') is None:
            popen_kwargs['stdout'] = subprocess.PIPE
        if popen_kwargs.get('stderr') is None:
            popen_kwargs['stderr'] = subprocess.PIPE

        try:
            normcmd = ProcHelper.which(command, ProcHelper.augmented_env['PATH'])
            if normcmd is not None:
                self.process = subprocess.Popen(normcmd
                                                , stdin=subprocess.PIPE
                                                , env=ProcHelper.augmented_env
                                                , **popen_kwargs
                                               )

                self.process.stdin.write(Utils.encode_bytes(input_string))
            else:
                self.process = None
                self.process_err = "SublimeHaskell.ProcHelper: {0} was not found on PATH!".format(command[0])

        except OSError as os_exc:
            self.process_err = \
                '\n'.join(["SublimeHaskell: Problem executing '{0}'".format(' '.join(command))
                           , 'Operating system error: {0}'.format(os_exc)
                          ])

            if os_exc.errno == errno.EPIPE:
                # Most likely reason: subprocess output a usage message
                stdout, stderr = self.process.communicate()
                exit_code = self.process.wait()
                self.process_err = self.process_err + \
                    '\n'.join([''
                               , 'Process exit code: {0}'.format(exit_code)
                               , ''
                               , "output:"
                               , stdout if stdout and len(stdout) > 0 else "--no output--"
                               , ''
                               , 'error:'
                               , stderr if stderr and len(stderr) > 0 else "--no error output--"])
                self.process = None
            else:
                self.process = None
                raise os_exc

    # 'with' statement support:
    def __enter__(self):
        return self

    def __exit__(self, _type, _value, _traceback):
        self.cleanup()
        return False

    def cleanup(self):
        if self.process is not None:
            self.process.stdin.close()
            self.process.stdout.close()
            if self.process.stderr is not None:
                # stderr can be None if it is tied to stdout (i.e., 'stderr=subprocess.STDOUT')
                self.process.stderr.close()

    def wait(self):
        """Wait for subprocess to complete and exit, collect and decode ``stdout`` and ``stderr``,
        returning the tuple ``(exit_code, stdout, stderr)```"""
        if self.process is not None:
            stdout, stderr = self.process.communicate()
            exit_code = self.process.wait()
            # Ensure that we reap the file descriptors.
            self.cleanup()
            return (exit_code, Utils.decode_bytes(stdout), Utils.decode_bytes(stderr))
        else:
            return (-1, '', self.process_err or "?? unknown error -- no process.")

    # Update the augmented environment when `add_to_PATH` or `add_standard_dirs` change.
    @staticmethod
    def update_environment(_key, _val):
        # Reinitialize the tool -> path cache:
        with ProcHelper.which_cache as cache:
            cache = {}
        ProcHelper.augmented_env = ProcHelper.get_extended_env()

    # Generate the augmented environment for subprocesses. This copies the
    # current process environment and updates PATH with `add_to_PATH` extras.
    @staticmethod
    def get_extended_env():
        def normalize_path(dir):
            return os.path.normpath(os.path.expandvars(os.path.expanduser(dir)))

        def cabal_config():
            cconfig = os.environ.get('CABAL_CONFIG') or \
                ('~/.cabal' if not isWinXX() else '%APPDATA%/cabal') + \
                "/config"

            # Various patterns to match...
            re_user_dirs = re.compile(r'^install-dirs\s+user')
            re_global_dirs = re.compile(r'^install-dirs\s+global')
            re_section = re.compile(r'^\w+')
            re_prefix = re.compile(r'prefix:\s+(.*)$')
            re_bindir = re.compile(r'bindir:\s+(.*)$')

            # Things to collect
            user_prefix = "$HOME/.cabal" if not isWinXX() else "%APPDATA%/cabal"
            # FIXME: Need to interrogate Shel32 for the Windows PROGRAMFILES known
            # folder path:
            global_prefix = "/usr/local" if not isWinXX() else "%PROGRAMFILES%/Haskell"
            user_bindir = "bin"
            global_bindir = "bin"
            p_state = 0

            try:
                with open(normalize_path(cconfig), 'rU') as f_cconfig:
                    # You would think that the Cabal maintainers would use a
                    # well known file format... But now, they didn't. And they
                    # had to go with an indentation-specific format.
                    #
                    # This is a "cheap and dirty" scanner to pick up
                    for l in f_cconfig:
                        l1 = l.rstrip()
                        # One of the sections?
                        if re_user_dirs.match(l1):
                            p_state = 1
                        elif re_global_dirs.match(l1):
                            p_state = 2
                        elif re.match('^\s+\w', l1):
                            # prefix attribute?
                            m = re_prefix.search(l1)
                            if m:
                                if p_state == 1:
                                    user_prefix = m.group(1)
                                elif p_state == 2:
                                    global_prefix = m.group(1)
                            # bindir attribute?
                            m = re_bindir.search(l1)
                            if m:
                                if p_state == 1:
                                    user_bindir = m.group(1)
                                elif p_state == 2:
                                    global_bindir = m.group(1)
                        elif re_section.match(l1):
                            p_state = 0

            except IOError as e:
                # Silently fail.
                pass

            return [os.path.join(user_prefix, user_bindir)
                    , os.path.join(global_prefix, global_bindir)
                   ]

        ext_env = dict(os.environ)
        PATH = os.getenv('PATH') or ""
        std_places = []
        if Settings.get_setting_async('add_standard_dirs', True):
            std_places = ["$HOME/.local/bin" if not isWinXX() else "%APPDATA%/local/bin"] + cabal_config()
            std_places = list(filter(os.path.isdir, map(normalize_path, std_places)))

        add_to_PATH = list(map(normalize_path, Settings.get_setting_async('add_to_PATH', [])))
        if not Utils.PyV3:
            # convert unicode strings to strings (for Python < 3). Environment
            # can contain only strings.
            add_to_PATH = list(map(str, add_to_PATH))

        add_to_PATH = list(filter(os.path.isdir, add_to_PATH))

        print("std_places = {0}".format(std_places))
        print("add_to_PATH = {0}".format(add_to_PATH))

        ext_env['PATH'] = os.pathsep.join(add_to_PATH + std_places + [PATH])
        return ext_env

    @staticmethod
    def which(args, env_path):
        def is_exe(fpath):
            return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

        with ProcHelper.which_cache as c:
            cval = c.get(args[0])

        if cval is not None:
            return [cval] + args[1:]
        else:
            exeExts = [''] if not isWinXX() else ['.exe', '.cmd', '.bat']

            program = args[0]
            fpath, fname = os.path.split(program)
            if fpath:
                if is_exe(program):
                    return args
            else:
                for path in env_path.split(os.pathsep):
                    path = path.strip('"')
                    for ext in exeExts:
                        exe_file = os.path.join(path, program)
                        if is_exe(exe_file + ext):
                            with ProcHelper.which_cache as c:
                                c[program] = exe_file
                            return [exe_file] + args[1:]

        return None

    @staticmethod
    def run_process(command, input_string='', **popen_kwargs):
        """Execute a subprocess, wait for it to complete, returning a ``(exit_code, stdout, stderr)``` tuple."""
        with ProcHelper(command, input_string, **popen_kwargs) as proc:
            return proc.wait()

    @staticmethod
    def invoke_tool(command, tool_name, input='', on_result=None, filename=None, on_line=None, check_enabled=True, **popen_kwargs):
        if check_enabled and not Settings.get_setting_async(Utils.tool_enabled(tool_name)):
            return None

        source_dir = Common.get_source_dir(filename)

        def mk_result(s):
            return on_result(s) if on_result else s

        try:
            with ProcHelper(command, input, cwd = source_dir, **popen_kwargs) as p:
                exit_code, stdout, stderr = p.wait()
                if exit_code != 0:
                    raise Exception('{0} exited with exit code {1} and stderr: {2}'.format(tool_name, exit_code, stderr))

                if on_line:
                    for l in io.TextIOWrapper(stdout, encoding='utf-8'):
                        on_line(mk_result(l))
                else:
                    return mk_result(io.TextIOWrapper(stdout, encoding='utf-8'))

        except OSError as e:
            if e.errno == errno.ENOENT:
                Common.output_error_async(sublime.active_window(), "SublimeHaskell: {0} was not found!\n'{1}' is set to False".format(tool_name, Utils.tool_enabled(tool_name)))
                Settings.set_setting_async(Utils.tool_enabled(tool_name), False)
            else:
                Logging.log('{0} fails with {1}, command: {2}'.format(tool_name, e, command), Logging.LOG_ERROR)

            return None

        except Exception as e:
            Logging.log('{0} fails with {1}, command: {2}'.format(tool_name, e, command), Logging.LOG_ERROR)

        return None


def get_source_dir(filename):
    """
    Get root of hs-source-dirs for filename in project
    """
    if not filename:
        return os.path.expanduser('~')
        # return os.getcwd()

    cabal_dir, _ = Common.get_cabal_project_dir_and_name_of_file(filename)
    if not cabal_dir:
        return os.path.dirname(filename)

    _, cabal_file = Common.get_cabal_in_dir(cabal_dir)
    exit_code, out, _ = ProcHelper.run_process(['hsinspect', cabal_file])

    if exit_code == 0:
        info = json.loads(out)

        dirs = ["."]

        if 'error' not in info and 'description' in info:
            # collect all hs-source-dirs
            descr = info['description']
            if descr['library']:
                dirs.extend(descr['library']['info']['source-dirs'])
            for i in descr['executables']:
                dirs.extend(i['info']['source-dirs'])
            for test in descr['tests']:
                dirs.extend(test['info']['source-dirs'])

        paths = [os.path.abspath(os.path.join(cabal_dir, d)) for d in dirs]
        paths.sort(key=lambda p: -len(p))

        for path in paths:
            if filename.startswith(path):
                return path

    return os.path.dirname(filename)
