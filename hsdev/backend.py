"""
The `hsdev` backend.
"""

from functools import reduce
import io
import os
import re
import threading

import sublime

import SublimeHaskell.hsdev.client as HsDevClient
import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.which as Which
import SublimeHaskell.sublime_haskell_common as Common
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


def use_hsdev(def_val=None):
    """Return a default value if hsdev is not enabled/connected
    """
    def decorator(use_fn):
        def inner(self, *args, **kwargs):
            if self.main_client.is_connected() and self.aux_client.is_connected():
                return use_fn(self, *args, **kwargs)
            else:
                return def_val
        return inner
    return decorator


class HsDevBackend(Backend.HaskellBackend):
    """This class encapsulates all of the functions that interact with the `hsdev` backend.
    """

    HSDEV_DEFAULT_PORT = 4567
    HSDEV_DEFAULT_HOST = 'localhost'
    HSDEV_MIN_VER = [0, 2, 0, 0]  # minimum hsdev version
    HSDEV_MAX_VER = [0, 2, 3, 0]  # maximum hsdev version

    def __init__(self):
        super().__init__()
        # Local hsdev server process and params
        self.is_local_hsdev = Settings.PLUGIN.hsdev_local_process
        self.hsdev_process = None
        self.cache = os.path.join(Common.sublime_haskell_cache_path(), 'hsdev')
        self.log_file = os.path.join(Common.sublime_haskell_cache_path(), 'hsdev', 'hsdev.log')
        self.log_config = Settings.PLUGIN.hsdev_log_config
        self.drain_stdout = None
        self.drain_stderr = None
        # Connection params
        self.port = Settings.PLUGIN.hsdev_port or HsDevBackend.HSDEV_DEFAULT_PORT
        self.hostname = HsDevBackend.HSDEV_DEFAULT_HOST
        if not self.is_local_hsdev and Settings.PLUGIN.hsdev_host:
            self.hostname = Settings.PLUGIN.hsdev_host
        # Main client connection: usually synchronous
        self.main_client = None
        # Auxiliary client connection: asynchronous
        self.aux_client = None
        # Inspection thread
        self.inspector = None

    @staticmethod
    def backend_name():
        return 'hsdev'

    @staticmethod
    def is_available():
        hsdev_path = Which.which('hsdev', ProcHelper.ProcHelper.get_extended_env().get('PATH'))
        hsdev_ver = HsDevBackend.hsdev_version() if hsdev_path is not None else [0, 0, 0, 0]
        Logging.log('hsdev version: {0}'.format('.'.join(map(str, hsdev_ver))), Logging.LOG_INFO)
        return hsdev_path is not None and \
               (hsdev_ver >= HsDevBackend.HSDEV_MIN_VER and hsdev_ver <= HsDevBackend.HSDEV_MAX_VER)

    def start_backend(self):
        retval = True
        if self.is_local_hsdev:
            Logging.log('Starting local \'hsdev\'server', Logging.LOG_INFO)

            cmd = self.concat_args([(True, ["hsdev", "run"]),
                                    (self.port, ["--port", str(self.port)]),
                                    (self.cache, ["--cache", self.cache]),
                                    (self.log_file, ["--log", self.log_file]),
                                    (self.log_config, ["--log-config", self.log_config])])

            Logging.log('hsdev command: {0}'.format(cmd), Logging.LOG_DEBUG)

            hsdev_proc = ProcHelper.ProcHelper(cmd)
            if hsdev_proc.process is not None:
                # Use TextIOWrapper here because it combines decoding with newline handling,
                # which means less to maintain.
                hsdev_proc.process.stdout = io.TextIOWrapper(hsdev_proc.process.stdout, 'utf-8')
                hsdev_proc.process.stderr = io.TextIOWrapper(hsdev_proc.process.stderr, 'utf-8')

                # Read and wait for hsdev's startup messge. 15 seconds should be enough time for the message to appear.
                # Otherwise, kill the thread because we don't want to get stuck waiting forever.
                startup_reader = HsDevStartupReader(hsdev_proc.process.stdout)
                startup_reader.start()
                startup_reader.wait_startup(15.0)
                if startup_reader.successful():
                    port = startup_reader.port()
                    if port != self.port:
                        Logging.log('hsdev: server port changed, was {0}, now {1}'.format(self.port, port), Logging.LOG_WARNING)
                        self.port = port
                    self.drain_stdout = OutputCollector.DescriptorDrain('hsdev stdout', hsdev_proc.process.stdout)
                    self.drain_stderr = OutputCollector.DescriptorDrain('hsdev stderr', hsdev_proc.process.stderr)
                    self.drain_stdout.start()
                    self.drain_stderr.start()
                    self.hsdev_process = hsdev_proc

                    Logging.log('Local \'hsdev\' server started successfully.', Logging.LOG_INFO)
                else:
                    # This is a bit of a "Hail Mary!" because readline() could just hang forever. Just to make sure,
                    # kill the process too!
                    startup_reader.stop()
                    hsdev_proc.process.kill()
                    self.hsdev_process = None
                    retval = False

                    sublime.error_message('Timed out waiting for \'hsdev\' to start up.')
            else:
                errmsg = 'Could not start local \'hsdev\' server because:\n\n' + hsdev_proc.process_err
                sublime.error_message(errmsg)
                self.hsdev_process = None
                retval = False

        return retval

    def connect_backend(self):
        Logging.log('Connecting to \'hsdev\' server at {0}:{1}'.format(self.hostname, self.port), Logging.LOG_INFO)
        retval = True
        self.main_client = HsDevClient.HsDevClient(self.hostname, self.port)
        self.aux_client = HsDevClient.HsDevClient(self.hostname, self.port)
        if self.main_client.connect() and self.aux_client.connect():
            # For a local hsdev server that we started, send the link command so that it exits when we exit.
            if self.is_local_hsdev:
                self.main_client.link()
            # Start the inspection process...
            # FIXME: Settings.PLUGIN.add_change_callback('inspect_modules', self.on_inspect_modules_changed)
            self.inspector = HsDevInspector(self.main_client, self.aux_client)
            self.inspector.start()
        else:
            Logging.log('Connections to \'hsdev\' server unsuccessful, see tracebacks to diagnose.', Logging.LOG_ERROR)
            retval = False
        return retval

    @staticmethod
    def hsdev_version():
        retval = None
        exit_code, out, _ = ProcHelper.ProcHelper.run_process(['hsdev', 'version'])

        if exit_code == 0:
            hsver = re.match(r'(?P<major>\d+)\.(?P<minor>\d+)\.(?P<revision>\d+)\.(?P<build>\d+)', out)
            if hsver:
                major = int(hsver.group('major'))
                minor = int(hsver.group('minor'))
                revision = int(hsver.group('revision'))
                build = int(hsver.group('build'))
                retval = [major, minor, revision, build]

        return retval


    def concat_args(self, args):
        def inner_concat(left, right):
            (left_pred, left_expr) = left
            (right_pred, right_expr) = right
            return (left_pred or right_pred, (left_expr if left_pred else []) + (right_expr if right_pred else []))

        return reduce(inner_concat, args, (True, []))[1]


class HsDevStartupReader(threading.Thread):
    '''Separate thread object that reads the local `hsdev` server's `stdout` looking for the server's startup
    message. The server's port number is parsed from the startup message and saved in the object's `hsdev_port`
    attribute, just in case this differs from the default or requested port.
    '''

    def __init__(self, fstdout):
        super().__init__(name='hsdev startup reader')
        self.stdout = fstdout
        self.hsdev_port = -1
        self.end_event = threading.Event()

    def run(self):
        self.end_event.clear()

        while not self.end_event.is_set():
            srvout = self.stdout.readline().strip()
            if srvout != '':
                Logging.log('hsdev initial output: {0}'.format(srvout), Logging.LOG_DEBUG)
                start_confirm = re.match(r'^.*?hsdev> Server started at port (?P<port>\d+)$', srvout)
                if start_confirm:
                    self.hsdev_port = int(start_confirm.group('port'))
                    Logging.log('\'hsdev\' server started at port {0}'.format(self.hsdev_port))
                    self.end_event.set()
            else:
                # Got EOF, stop loop.
                self.end_event.set()

    def wait_startup(self, tmo):
        self.end_event.wait(tmo)

    def successful(self):
        return self.end_event.is_set()

    def stop(self):
        self.end_event.clear()

    def port(self):
        return self.hsdev_port

class HsDevInspector(threading.Thread):
    '''The inspection thread.
    '''

    # Re-inspect event wait time, in seconds
    WAIT_TIMEOUT = 60.0

    def __init__(self, main_client, aux_client):
        super().__init__(name='hsdev inspector')
        # Thread control event
        self.end_event = threading.Event()
        # Connection state
        self.main_client = main_client
        self.aux_client = aux_client
        # (Re-)Inspection state:
        self.dirty_lock = threading.Lock()
        self.cabal_to_load = LockedObject.LockedObject([])
        self.dirty_files = LockedObject.LockedObject([])
        self.dirty_paths = LockedObject.LockedObject([])
        self.reinspect_event = threading.Event()

    def run(self):
        self.end_event.clear()
        while not self.end_event.is_set():
            if not self.main_client.ping():
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
                self.aux_client.docs(files=files_to_reinspect)

            self.reinspect_event.wait(HsDevInspector.WAIT_TIMEOUT)
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

    @use_hsdev()
    def inspect_cabal(self, cabal=None):
        with Common.status_message_process('Inspecting {0}'.format(cabal or 'cabal'), priority=1) as smgr:
            self.aux_client.scan(cabal=(cabal == 'cabal'),
                                 sandboxes=[] if cabal == 'cabal' else [cabal],
                                 on_notify=ScanStatus(smgr),
                                 wait=True,
                                 docs=Settings.PLUGIN.enable_hdocs)

    @use_hsdev()
    @use_inspect_modules
    def inspect(self, paths, projects, files):
        if paths or projects or files:
            with Common.status_message_process('Inspecting', priority=1) as smgr:
                self.aux_client.scan(paths=paths,
                                     projects=projects,
                                     files=files,
                                     on_notify=ScanStatus(smgr),
                                     wait=True,
                                     ghc=Settings.PLUGIN.ghc_opts,
                                     docs=Settings.PLUGIN.enable_hdocs)

    @use_hsdev()
    @use_inspect_modules
    def inspect_path(self, path):
        with Common.status_message_process('Inspecting path {0}'.format(path), priority=1) as smgr:
            self.aux_client.scan(paths=[path],
                                 on_notify=ScanStatus(smgr),
                                 wait=True,
                                 ghc=Settings.PLUGIN.ghc_opts,
                                 docs=Settings.PLUGIN.enable_hdocs)

    @use_hsdev()
    @use_inspect_modules
    def inspect_project(self, cabal_dir):
        (project_name, _) = Common.get_cabal_in_dir(cabal_dir)

        with Common.status_message_process('Inspecting project {0}'.format(project_name), priority=1) as smgr:
            self.aux_client.scan(projects=[cabal_dir],
                                 on_notify=ScanStatus(smgr),
                                 wait=True,
                                 docs=Settings.PLUGIN.enable_hdocs)

    @use_hsdev()
    @use_inspect_modules
    def inspect_files(self, filenames):
        with Common.status_message_process('Inspecting files', priority=1) as smgr:
            self.aux_client.scan(files=filenames,
                                 on_notify=ScanStatus(smgr),
                                 wait=True,
                                 ghc=Settings.PLUGIN.ghc_opts,
                                 docs=Settings.PLUGIN.enable_hdocs)
