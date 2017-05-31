
import errno
import os
import re
import subprocess

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.proc_helper as ProcHelper


def call_ghcmod_and_wait(arg_list, filename=None, cabal=None):
    """
    Calls ghc-mod with the given arguments.
    Shows a sublime error message if ghc-mod is not available.
    """

    ghc_opts_args = get_ghc_opts_args(filename, add_package_db=False, cabal=cabal)
    command = ['ghc-mod', '-b', '\n'] + ghc_opts_args + arg_list

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

    # print('call_ghcmod_and_wait: out = {0}'.format(out))
    # print('call_ghcmod_and_wait: err = {0}'.format(err))

    if exit_code != 0:
        raise Exception("%s exited with status %d and stderr: %s" % (' '.join(command), exit_code, err))

    return out


def ghcmod_type(filename, module_name, line, column, cabal=None):
    """
    Uses ghc-mod type to infer type
    """
    return call_ghcmod_and_wait(['type', filename, module_name, str(line), str(column)], filename=filename, cabal=cabal)


## Unreferenced function:
##
# def ghcmod_info(filename, module_name, symbol_name, cabal=None):
#     """
#     Uses ghc-mod info filename module_name symbol_name to get symbol info
#     """
#     contents = call_ghcmod_and_wait(['info', filename, module_name, symbol_name], filename=filename, cabal=cabal)
#     # TODO: Returned symbol doesn't contain location
#     # But in fact we use ghcmod_info only to retrieve type of symbol
#     return ParseOutput.parse_info(symbol_name, contents)
