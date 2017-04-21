
import threading

import sublime

import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.worker as Worker


# Show scan progress in status bar
class ScanStatus(object):
    def __init__(self, status_message):
        self.status_message = status_message

    def __call__(self, msgs):
        statuses = []
        for msg in msgs:
            progress = msg['progress']
            statuses.append('{0} ({1}/{2})'.format(msg['name'], progress['current'], progress['total'])
                            if progress else msg['name'])
        self.status_message.change_message('Inspecting {0}'.format(' / '.join(statuses)))


# Set reinspect event
def dirty(dirty_fn):
    def wrapped(self, *args, **kwargs):
        acquired = self.dirty_lock.acquire(blocking=False)
        try:
            return dirty_fn(self, *args, **kwargs)
        finally:
            if acquired:
                self.dirty_lock.release()
                self.reinspect_event.set()
    return wrapped


def use_inspect_modules(inspect_fn):
    def wrapped(self, *args, **kwargs):
        if Settings.PLUGIN.inspect_modules:
            return inspect_fn(self, *args, **kwargs)
    return wrapped


# You know, Barn... it's very tempting to name this "InspectorFish" (if you grew up with "Barney Miller." :-)
class Inspector(threading.Thread):
    '''The inspection thread.
    '''

    # Re-inspect event wait time, in seconds
    WAIT_TIMEOUT = 60.0

    def __init__(self, backend):
        super().__init__(name='hsdev inspector')
        # Thread control event
        self.end_event = threading.Event()
        # The backend, whose support functions we invoke:
        self.backend = backend
        # (Re-)Inspection state:
        self.dirty_lock = threading.Lock()
        self.cabal_to_load = LockedObject.LockedObject([])
        self.dirty_files = LockedObject.LockedObject([])
        self.dirty_paths = LockedObject.LockedObject([])
        self.reinspect_event = threading.Event()

    def run(self):
        self.end_event.clear()
        while not self.end_event.is_set():
            if not self.backend.ping():
                Logging.log('hsdev ping: no pong', Logging.LOG_WARNING)

            scan_paths = []
            with self.dirty_paths as dirty_paths:
                scan_paths = dirty_paths[:]
                dirty_paths[:] = []

            files_to_reinspect = []
            with self.dirty_files as dirty_files:
                files_to_reinspect = dirty_files[:]
                dirty_files[:] = []

            projects = []
            files = []

            if len(files_to_reinspect) > 0:
                projects = []
                files = []
                for finspect in files_to_reinspect:
                    projdir = Common.get_cabal_project_dir_of_file(finspect)
                    if projdir is not None:
                        projects.append(projdir)
                    else:
                        files.append(finspect)

            projects = list(set(projects))
            files = list(set(files))

            self.inspect(paths=scan_paths, projects=projects, files=files)

            load_cabal = []
            with self.cabal_to_load as cabal_to_load:
                load_cabal = cabal_to_load[:]
                cabal_to_load[:] = []

            for cabal in load_cabal:
                Worker.run_async('inspect cabal {0}'.format(cabal), self.inspect_cabal, cabal)

            if files_to_reinspect and Settings.PLUGIN.enable_hdocs:
                self.backend.docs(files=files_to_reinspect)

            self.reinspect_event.wait(Inspector.WAIT_TIMEOUT)
            self.reinspect_event.clear()

    @dirty
    def force_inspect(self):
        self.reinspect_event.set()

    @dirty
    def start_inspect(self):
        self.mark_cabal()
        self.mark_all_files()

    @dirty
    @use_inspect_modules
    def mark_all_files(self):
        for window in sublime.windows():
            with self.dirty_files as dirty_files:
                dirty_files.extend(list(filter(lambda f: f and f.endswith('.hs'), [v.file_name() for v in window.views()])))
            with self.dirty_paths as dirty_paths:
                dirty_paths.extend(window.folders())

    @dirty
    @use_inspect_modules
    def mark_file_dirty(self, filename):
        if filename is not None:
            with self.dirty_files as dirty_files:
                dirty_files.append(filename)

    @dirty
    def mark_cabal(self, cabal_name=None):
        with self.cabal_to_load as cabal_to_load:
            cabal_to_load.append(cabal_name or 'cabal')

    def inspect_cabal(self, cabal=None):
        with Common.status_message_process('Inspecting {0}'.format(cabal or 'cabal'), priority=1) as smgr:
            self.backend.scan(cabal=(cabal == 'cabal'),
                              sandboxes=[] if cabal == 'cabal' else [cabal],
                              on_notify=ScanStatus(smgr),
                              wait=True,
                              docs=Settings.PLUGIN.enable_hdocs)

    @use_inspect_modules
    def inspect(self, paths, projects, files):
        if paths or projects or files:
            with Common.status_message_process('Inspecting', priority=1) as smgr:
                self.backend.scan(paths=paths,
                                  projects=projects,
                                  files=files,
                                  on_notify=ScanStatus(smgr),
                                  wait=True,
                                  ghc=Settings.PLUGIN.ghc_opts,
                                  docs=Settings.PLUGIN.enable_hdocs)

    @use_inspect_modules
    def inspect_path(self, path):
        with Common.status_message_process('Inspecting path {0}'.format(path), priority=1) as smgr:
            self.backend.scan(paths=[path],
                              on_notify=ScanStatus(smgr),
                              wait=True,
                              ghc=Settings.PLUGIN.ghc_opts,
                              docs=Settings.PLUGIN.enable_hdocs)

    @use_inspect_modules
    def inspect_project(self, cabal_dir):
        (project_name, _) = Common.get_cabal_in_dir(cabal_dir)

        with Common.status_message_process('Inspecting project {0}'.format(project_name), priority=1) as smgr:
            self.backend.scan(projects=[cabal_dir],
                              on_notify=ScanStatus(smgr),
                              wait=True,
                              docs=Settings.PLUGIN.enable_hdocs)

    @use_inspect_modules
    def inspect_files(self, filenames):
        with Common.status_message_process('Inspecting files', priority=1) as smgr:
            self.backend.scan(files=filenames,
                              on_notify=ScanStatus(smgr),
                              wait=True,
                              ghc=Settings.PLUGIN.ghc_opts,
                              docs=Settings.PLUGIN.enable_hdocs)
