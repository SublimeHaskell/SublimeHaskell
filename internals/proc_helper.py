# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
# ProcHelper: Process execution helper class.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

import errno
import subprocess
import os
import os.path

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.internals.which as Which
import SublimeHaskell.internals.cabal_cfgrdr as CabalConfigRdr
import SublimeHaskell.internals.cabal_reader as CabalReader

class ProcHelper(object):
    """Command and tool process execution helper."""

    # Augmented PATH for the subprocesses and locating executables.
    augmented_path = None

    def __init__(self, command, **popen_kwargs):
        """Open a pipe to a command or tool."""

        if ProcHelper.augmented_path is None:
            ProcHelper.augmented_path = ProcHelper.make_augmented_path()

        ## Necessary evil: Don't cache the environment, just update the PATH in the current environment.
        ## Why? Because someone could (like me) change os.environ via the ST console and those changes
        ## would never make it here. Use case: settting $http_proxy so that stack can fetch packages.
        proc_env = dict(os.environ)
        proc_env['PATH'] = ProcHelper.augmented_path + os.pathsep + proc_env.get('PATH', '')

        self.process = None
        self.process_err = None

        if Utils.is_windows():
            startupinfo = subprocess.STARTUPINFO()
            startupinfo.dwFlags |= subprocess.STARTF_USESHOWWINDOW
            popen_kwargs['startupinfo'] = startupinfo

        # Allow caller to specify something different for stdout or stderr -- provide
        # the default here if unspecified.
        popen_kwargs['stdout'] = popen_kwargs.get('stdout', subprocess.PIPE)
        popen_kwargs['stderr'] = popen_kwargs.get('stderr', subprocess.PIPE)

        try:
            normcmd = Which.which(command, proc_env['PATH'])
            if normcmd is not None:
                self.process = subprocess.Popen(normcmd, stdin=subprocess.PIPE, env=proc_env, **popen_kwargs)
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
                               , stdout if stdout else "--no output--"
                               , ''
                               , 'error:'
                               , stderr if stderr else "--no error output--"])
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

    def wait(self, input_str=None):
        """Wait for subprocess to complete and exit, collect and decode ``stdout`` and ``stderr``,
        returning the tuple ``(exit_code, stdout, stderr)```"""
        if self.process is not None:
            stdout, stderr = self.process.communicate(Utils.encode_bytes(input_str if input_str else ''))
            exit_code = self.process.wait()
            # Ensure that we reap the file descriptors.
            self.cleanup()
            return (exit_code, Utils.decode_bytes(stdout), Utils.decode_bytes(stderr))

        return (-1, '', self.process_err or "?? unknown error -- no process.")

    # Update the augmented environment when `add_to_PATH` or `add_standard_dirs` change.
    @staticmethod
    def update_environment(_key, _val):
        # Reinitialize the tool -> path cache:
        Which.reset_cache()
        ProcHelper.augmented_path = ProcHelper.make_augmented_path()

    @staticmethod
    def make_augmented_path():
        ''' Generate the augmented PATH for subprocesses: adds the appropriate cabal/stack local install directory
        ($HOME/.local/bin for *nix, %APPDATA%/local/bin for Windows) and updates PATH with `add_to_PATH` extras.
        '''
        std_places = []
        if Settings.PLUGIN.add_standard_dirs:
            std_places.append("$HOME/.local/bin" if not Utils.is_windows() else "%APPDATA%/local/bin")
            if Utils.is_macosx():
                std_places.append('$HOME/Library/Haskell/bin')
            std_places += CabalConfigRdr.cabal_config()
            std_places = [dir for dir in [Utils.normalize_path(path) for path in std_places] if os.path.isdir(dir)]

        add_to_path = list(filter(os.path.isdir, map(Utils.normalize_path, Settings.PLUGIN.add_to_path)))

        Logging.log("std_places = {0}".format(std_places), Logging.LOG_INFO)
        Logging.log("add_to_PATH = {0}".format(add_to_path), Logging.LOG_INFO)

        return os.pathsep.join(add_to_path + std_places)

    @staticmethod
    def get_extended_path():
        if ProcHelper.augmented_path is None:
            ProcHelper.augmented_path = ProcHelper.make_augmented_path()
        return ProcHelper.augmented_path + os.pathsep + (os.environ.get('PATH', ''))

    @staticmethod
    def run_process(command, input_string='', **popen_kwargs):
        """Execute a subprocess, wait for it to complete, returning a ``(exit_code, stdout, stderr)``` tuple."""
        with ProcHelper(command, **popen_kwargs) as proc:
            return proc.wait(input_string)


def exec_wrapper_cmd(exec_with, cmd_list):
    wrapper = []
    if exec_with == 'cabal':
        wrapper = ['cabal', 'exec', cmd_list[0]]
    elif exec_with == 'cabal-new-build':
        wrapper = ['cabal', 'new-run', 'exe:' + cmd_list[0]]
    elif exec_with == 'stack':
        wrapper = ['stack', 'exec', cmd_list[0]]
    else:
        errmsg = 'ProcHelper.exec_wrapper_cmd: Unknown execution prefix \'{0}\''.format(exec_with)
        raise RuntimeError(errmsg)

    return wrapper + ['--'] + cmd_list[1:] if cmd_list[1:] else wrapper

def exec_with_wrapper(exec_with, install_dir, cmd_list):
    '''Wrapper function for inserting the execution wrapper, e.g., 'cabal exec' or 'stack exec'

    :returns: Process object from ProcHelper.
    '''

    proc_args = {}
    if exec_with is not None:
        cmd_list = exec_wrapper_cmd(exec_with, cmd_list)
        if install_dir is not None:
            proc_args['cwd'] = Utils.normalize_path(install_dir)
        else:
            raise RuntimeError('ProcHelper.exec_with_wrapper: invalid install_dir (None)')
    else:
        cmd = Which.which(cmd_list[0], ProcHelper.get_extended_path())
        if cmd is not None:
            cmd_list[0] = cmd

    Logging.log('ProcHelper.exec_with_wrapper: {0} in {1}'.format(cmd_list, proc_args.get('cwd')), Logging.LOG_DEBUG)
    return ProcHelper(cmd_list, **proc_args)


def get_source_dir(filename):
    '''Get root of hs-source-dirs for filename in project.
    '''
    if not filename:
        return os.path.expanduser('~')

    cabal_dir, cabal_proj = Common.locate_cabal_project(filename)
    if not cabal_dir:
        # No cabal file -> Punt and assume the source directory for the file and project is the same as the file.
        return os.path.dirname(filename)
    else:
        proj_info = CabalReader.CabalProjectReader(cabal_dir, cabal_proj)
        cabal_info = proj_info.cabal_info
        dirs = ['.']

        executables = cabal_info.get('executable', {})
        dirs.extend([sdir.strip()
                     for exe in executables
                     for sdirs in executables[exe].get('hs-source-dirs', [])
                     for sdir in sdirs.split(',')])
        dirs.extend([sdir.strip()
                     for sdirs in cabal_info.get('library', {}).get('hs-source-dirs', [])
                     for sdir in sdirs.split(',')])

        paths = [os.path.abspath(os.path.join(cabal_dir, srcdirs)) for srcdirs in set(dirs)]
        paths.sort(key=lambda p: -len(p))

        for path in paths:
            if filename.startswith(path):
                return path

    return os.path.dirname(filename)
