import os
import os.path

import SublimeHaskell.internals.atomics as Atomics
import SublimeHaskell.internals.utils as Utils

def is_exe(fpath):
    return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

# Tool name -> executable path cache. Avoids probing the file system multiple times.
WHICH_CACHE = Atomics.AtomicDuck()

def which(cmd, env_path):
    cmd_is_list = isinstance(cmd, list)
    the_cmd = cmd[0] if cmd_is_list else cmd
    cmd_args = cmd[1:] if cmd_is_list else []

    if os.path.isabs(the_cmd):
        return cmd

    with WHICH_CACHE as cache:
        cval = cache.get(the_cmd)

    if cval is not None:
        return [cval] + cmd_args if cmd_is_list else cval
    else:
        exe_exts = [''] if not Utils.is_windows() else ['.exe', '.cmd', '.bat']

        program = the_cmd
        fpath, _ = os.path.split(program)
        if fpath:
            if is_exe(program):
                return cmd
        else:
            for path in env_path.split(os.pathsep):
                path = path.strip('"')
                for ext in exe_exts:
                    exe_file = os.path.join(path, program)
                    if is_exe(exe_file + ext):
                        with WHICH_CACHE as cache:
                            cache[program] = exe_file
                        return [exe_file] + cmd_args if cmd_is_list else exe_file

    return None

def reset_cache():
    global WHICH_CACHE
    WHICH_CACHE = Atomics.AtomicDuck()
