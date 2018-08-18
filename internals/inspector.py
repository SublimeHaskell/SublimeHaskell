
import os.path

import sublime

import SublimeHaskell.internals.atomics as Atomics
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.sublime_haskell_common as Common


# Show scan progress in status bar
class ScanStatus(object):
    def __init__(self, status_message):
        self.status_message = status_message

    def __call__(self, msgs):
        statuses = []
        smsg = ''
        for msg in msgs:
            progress = msg['progress']
            smsg = '{0} ({1}/{2})'.format(msg['name'], progress['current'], progress['total']) if progress else msg['name']
            statuses.append(smsg)
        smsg = ' / '.join(statuses)
        self.status_message.change_message('Inspecting {0}'.format(smsg))


def use_inspect_modules(inspect_fn):
    def wrapped(self, *args, **kwargs):
        return inspect_fn(self, *args, **kwargs) if Settings.PLUGIN.inspect_modules else None
    return wrapped


# You know, Bawwwn... it's very tempting to name this "InspectorFish" (if you grew up with "Barney Miller." :-)
class Inspector(object):
    '''The source inspector.
    '''

    def __init__(self, backend):
        super().__init__()
        # The backend, whose support functions we invoke:
        self.backend = backend
        # (Re-)Inspection state:
        self.cabal_to_load = Atomics.AtomicList()
        self.dirty_files = Atomics.AtomicDuck()
        self.cabal_scanned = False
        self.busy = False

    def __enter__(self):
        return self

    def __exit__(self, exc_type, _exc_val, _exc_tb):
        if Settings.COMPONENT_DEBUG.inspection:
            print('{0}.__exit__: exc {1}'.format(type(self).__name__, exc_type))
        if exc_type is None:
            self.do_inspection()

        # Propagate the exception, if we have one.
        return False

    def do_inspection(self):
        self.busy = True
        try:
            projects = []
            files = []
            files_contents = {}

            if not self.cabal_scanned:
                self.cabal_scanned = True
                self.inspect_user_db()

            with self.dirty_files as dirty_files:
                if Settings.COMPONENT_DEBUG.inspection:
                    print('do_inspection: dirty_files: {0}'.format(dirty_files))

                for finspect in dirty_files.keys() or []:
                    projdir = Common.get_cabal_project_dir_of_file(finspect)
                    if projdir is not None:
                        projects.append(projdir)

                    files.append(finspect)

                projects = list(set(projects))
                files = list(set(files))

                files_contents = dict((file, content) for file, content in dirty_files.items() if content)
                dirty_files.clear()

            cand_cabals = []
            with self.cabal_to_load as cabals_to_load:
                cand_cabals = cabals_to_load
                del cabals_to_load[:]

            for cabal in cand_cabals:
                projdir = Common.get_cabal_project_dir_of_file(cabal)
                if projdir is not None:
                    projects.append(projdir)

            files = list(set(files))
            projects = list(set(projects))

            self.inspect(
                paths=[],
                files=list(set(files)),
                projects=list(set(projects)),
                contents=files_contents,
            )
        finally:
            self.busy = False

    def start_inspect(self):
        self.mark_cabal()
        self.mark_all_files()

    @use_inspect_modules
    def mark_all_files(self):
        for window in sublime.windows():
            with self.dirty_files as dirty_files:
                dirty_files.update([
                    (f, None) for f in [v.file_name() for v in window.views()]
                    if f and (f.endswith('.hs') or f.endswith('.hsc')) and f not in dirty_files
                ])
                Logging.log("dirty files: : {0}".format(dirty_files), Logging.LOG_DEBUG)

    @use_inspect_modules
    def mark_file_dirty(self, filename, contents=None):
        if filename:
            with self.dirty_files as dirty_files:
                dirty_files[filename] = contents

    @use_inspect_modules
    def mark_cabal_dirty(self, cabal):
        if cabal:
            with self.cabal_to_load as cand_cabals:
                cand_cabals.append(os.path.dirname(cabal))

    @use_inspect_modules
    def mark_cabal(self):
        '''Scam all open window views, adding actual cabal files  or those indirectly identified by the Haskell sources
        to the list of cabal files to inspect.
        '''
        self.cabal_scanned = False

        with self.cabal_to_load as cand_cabals:
            cand_cabals = []
            for window in sublime.windows():
                for view in window.views():
                    fname = view.file_name()
                    if fname is not None:
                        if fname.endswith('.cabal'):
                            cand_cabals.append(os.path.dirname(fname))
                        elif fname.endswith('.hs'):
                            proj_dir = Common.locate_cabal_project(fname)[0]
                            if proj_dir is not None:
                                cand_cabals.append(proj_dir)
            # Make the list of cabal files unique
            cand_cabals[:] = list(set(cand_cabals))

    def inspect_user_db(self):
        with Common.status_message_process('Inspecting user-db', priority=1) as smgr:
            self.backend.scan_package_dbs(
                ['global-db', 'user-db'],
                on_notify=ScanStatus(smgr),
                wait_complete=True,
                timeout=None,
            )
            smgr.result_ok()

    @use_inspect_modules
    def inspect(self, paths, projects, files, contents):
        if paths or projects or files:
            if Settings.COMPONENT_DEBUG.inspection:
                print('{0}.inspect:'.format(type(self).__name__))
                print('  :: paths: {0}'.format(paths))
                print('  :: projects: {0}'.format(projects))
                print('  :: files: {0}'.format(files))

            build_tool = Settings.PLUGIN.haskell_build_tool
            with Common.status_message_process('Inspecting', priority=1) as smgr:
                for project in projects:
                    self.backend.scan_project(
                        project=project,
                        build_tool=build_tool,
                        wait_complete=True,
                        timeout=None,
                        on_notify=ScanStatus(smgr),
                    )
                    smgr.result_ok()
            with Common.status_message_process('Inspecting', priority=1) as smgr:
                for file in files:
                    self.backend.scan_file(
                        file=file,
                        build_tool=build_tool,
                        wait_complete=True,
                        timeout=None,
                        on_notify=ScanStatus(smgr),
                    )
                    file_contents = contents.get(file, None)
                    if file_contents is not None:
                        self.backend.set_file_contents(
                            file,
                            file_contents,
                            wait_complete=True,
                            timeout=None,
                            on_notify=ScanStatus(smgr),
                        )

                    smgr.result_ok()

    def is_busy(self):
        return self.busy
