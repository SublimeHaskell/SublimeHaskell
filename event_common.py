import SublimeHaskell.backend_cmds as BackendCmds
import SublimeHaskell.check_lint as CheckAndLint
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common


def update_completions_async(autocompleter, project_name, files=None, drop_all=False):
    if drop_all:
        Utils.run_async('drop all completions', autocompleter.drop_completions_async)
    else:
        for file in files or []:
            Utils.run_async('{0}: drop completions'.format(file), autocompleter.drop_completions_async, file)

    for file in files or []:
        Utils.run_async('{0}: init completions'.format(file), autocompleter.generate_completions_cache,
                        project_name, file)


def assoc_to_project(view, backend_mgr, filename):
    ## Update file -> project tracking
    project_dir, project_name = Common.locate_cabal_project(filename)
    if Settings.COMPONENT_DEBUG.event_viewer:
        print('EventCommon.assoc_to_project project_name {1} project_dir {0}'.format(project_dir, project_name))
    if project_dir and project_name:
        backend_mgr.add_project_file(filename, project_name, project_dir)
    BackendCmds.cabal_project_status(view, backend_mgr)


def do_check_lint(view):
    if Settings.PLUGIN.enable_auto_check and Settings.PLUGIN.enable_auto_lint:
        return CheckAndLint.exec_check_and_lint(view)
    elif Settings.PLUGIN.enable_auto_check:
        return CheckAndLint.exec_check(view)
    elif Settings.PLUGIN.enable_auto_lint:
        return CheckAndLint.exec_lint(view)

    return False
