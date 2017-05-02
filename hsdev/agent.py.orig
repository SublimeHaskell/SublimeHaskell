# -*- coding: UTF-8 -*-

from functools import reduce
import io
import os
import re
import threading
import traceback

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.hsdev.client as HsDevClient
import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.worker as Worker


HSDEV_DEFAULT_PORT = 4567
HSDEV_MIN_VER = [0, 2, 0, 0]  # minimum hsdev version

def hsdev_version():
    retval = None
    try:
        exit_code, out, _ = ProcHelper.ProcHelper.run_process(['hsdev', 'version'])
        if exit_code == 0:
            hsver = re.match(r'(?P<major>\d+)\.(?P<minor>\d+)\.(?P<revision>\d+)\.(?P<build>\d+)', out)
            if hsver:
                major = int(hsver.group('major'))
                minor = int(hsver.group('minor'))
                revision = int(hsver.group('revision'))
                build = int(hsver.group('build'))
                retval = [major, minor, revision, build]
    except OSError:
        Logging.log('Could not get hsdev version, see console window traceback', Logging.LOG_ERROR)
        traceback.print_exc()

    return retval


def show_version(ver):
    return '.'.join(map(str, ver))


def check_version(ver, minimal):
    return ver is not None and ver >= minimal


def patch_simple_log(ver):
    return ver >= [0, 2, 3, 1]


def format_error_details(details):
    return ', '.join(['{}: {}'.format(k, v) for k, v in details.items()])


def wait_result(wait_fn, *args, **kwargs):
    wait_receive = threading.Event()
    result = {'result': None}

    on_resp = kwargs.get('on_response')
    on_err = kwargs.get('on_error')

    def wait_response(resp):
        result['result'] = resp
        if on_resp:
            on_resp(resp)
        wait_receive.set()

    def wait_error(func_name, details):
        Logging.log('hsdev call fails with: {0}, {1}'.format(func_name, format_error_details(details)))
        if on_err:
            on_err(func_name, details)
        wait_receive.set()

    timeout = kwargs.pop('timeout', 0.1)

    kwargs['on_response'] = wait_response
    kwargs['on_error'] = wait_error

    wait_fn(*args, **kwargs)

    wait_receive.wait(timeout)
    return result['result']


def concat_args(args):
    def inner_concat(left, right):
        (left_pred, left_expr) = left
        (right_pred, right_expr) = right
        return (left_pred or right_pred, (left_expr if left_pred else []) + (right_expr if right_pred else []))

    return reduce(inner_concat, args, (True, []))[1]


class HsDevProcess(threading.Thread):
    """A container for the `hsdev` process, when running `hsdev` locally. Handles draining `hsdev`'s `stdout` and
    `stderr` file objects -- `hsdev` can be quite chatty, which leads to deadlock when `hsdev` has filled the pipe
    between SublimeText and itself.
    """
    def __init__(self, port, version, cache=None, log_file=None, log_config=None):
        super().__init__()
        self.process = None
        self.drain_stdout = None
        self.drain_stderr = None
        self.on_start = None
        self.on_exit = None
        self.stop_event = threading.Event()
        self.create_event = threading.Event()
        self.port = port
        self.cache = cache
        self.log_file = log_file
        self.log_config = log_config
        self.version = version

    def run(self):
        new_simple_log = patch_simple_log(self.version)

        cmd = concat_args([(True, ["hsdev", "run"]),
                           (self.port, ["--port", str(self.port)]),
                           (self.cache, ["--cache", self.cache]),
                           (self.log_file, ["--log", self.log_file]),
                           # HACK! HACK! HACK! Alexandr changed simple-log's command line API.
                           #
                           # "--log-config" only applies to earlier hsdev versions that support 'log politics' (and for those
                           # of us old enough to remember the Soviet era, 'log politics' is an incredibly Soviet notion that
                           # makes us smile! :-)
                           #
                           # Versions 0.2.3.0 and later: Simplified argument -- just the log level, and only one log level,
                           # Yvgeny! (With apologies to "The Hunt for Red October".)
                           (not new_simple_log and self.log_config, ["--log-config", self.log_config]),
                           (new_simple_log and self.log_config, ["--log-level", self.log_config])])

        Logging.log('hsdev command: {0}'.format(cmd), Logging.LOG_DEBUG)

        while True:
            self.create_event.wait()
            self.create_event.clear()
            while not self.stop_event.is_set():
                Logging.log('Starting hsdev server', Logging.LOG_INFO)
                hsdev_proc = ProcHelper.ProcHelper(cmd)
                if hsdev_proc.process is None:
                    Logging.log('Failed to create hsdev process', Logging.LOG_ERROR)
                    return

                # Use TextIOWrapper here because it combines decoding with newline handling,
                # which means less to maintain.
                hsdev_proc.process.stdout = io.TextIOWrapper(hsdev_proc.process.stdout, 'utf-8')
                hsdev_proc.process.stderr = io.TextIOWrapper(hsdev_proc.process.stderr, 'utf-8')

                while True:
                    srvout = hsdev_proc.process.stdout.readline().strip()
                    if srvout != '':
                        Logging.log('hsdev initial output: {0}'.format(srvout), Logging.LOG_DEBUG)
                        start_confirm = re.search(r'[Ss]erver started at port (?P<port>\d+)$', srvout)
                        if start_confirm:
                            Logging.log('hsdev server started at port {0}'.format(start_confirm.group('port')))
                            break
                    else:
                        Logging.log('hsdev initial output: Got EOF. Server not started successfully.')
                        return

                self.process = hsdev_proc.process
                self.drain_stdout = OutputCollector.DescriptorDrain('hsdev stdout', self.process.stdout)
                self.drain_stderr = OutputCollector.DescriptorDrain('hsdev stderr', self.process.stderr)
                self.drain_stdout.start()
                self.drain_stderr.start()
                HsCallback.call_callback(self.on_start, name='HsDevProcess.on_start')

                self.process.wait()

                self.drain_stdout.stop()
                self.drain_stderr.stop()
                HsCallback.call_callback(self.on_exit, name='HsDevProcess.on_exit')

            self.stop_event.clear()

    def active(self):
        return self.process.poll() is None

    def inactive(self):
        return self.process.poll() is not None

    def create(self):
        self.create_event.set()

    def stop(self):
        self.stop_event.set()

### HACK ALERT: If agent, client and client_back are already present in the
### module's globals, don't redefine them. Otherwise, you will end up with
### multiple instances of hsdev if the plugin is reloaded.

# global hsdev agent
if 'agent' not in globals():
    agent = None
else:
    agent = globals()['agent']
# global hsdev agent's hsdev client for command tasks
if 'client' not in globals():
    client = None
else:
    client = globals()['client']
# global hsdev agent's hsdev client for background tasks (commonly scanning)
if 'client_back' not in globals():
    client_back = None
else:
    client_back = globals()['client_back']


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
        if not hasattr(self, 'dirty_lock'):
            self.dirty_lock = threading.Lock()
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


def agent_connected():
    return agent.is_connected()


def use_hsdev(def_val=None):
    """Return a default value if hsdev is not enabled/connected
    """
    def wrap(use_fn):
        def wrapped(*args, **kwargs):
            if Settings.PLUGIN.enable_hsdev and agent_connected():
                return use_fn(*args, **kwargs)
            else:
                return def_val
        return wrapped
    return wrap


class HsDevAgent(threading.Thread):
    """Base class for managing the local or remote `hsdev` agent that does work for the plugin.
    """
    def __init__(self, host, port=HSDEV_DEFAULT_PORT):
        super().__init__()
        self.daemon = True
        self.cabal_to_load = LockedObject.LockedObject([])
        self.dirty_files = LockedObject.LockedObject([])
        self.dirty_paths = LockedObject.LockedObject([])
        self.client = HsDevClient.HsDev(host, port)
        self.client_back = HsDevClient.HsDev(host, port)
        self.reinspect_event = threading.Event()

    def start_hsdev(self):
        """Start a connection to an `hsdev` server. Utilizes a protocol call sequence: the subclass provides its specific
        startup implementation via the `do_hsdev_start` method so that calls to `super()` are avoided.
        """
        def connected_():
            Logging.log('hsdev agent: primary connection to hsdev established', Logging.LOG_TRACE)

        def back_connected_():
            Logging.log('hsdev agent: async connection to hsdev established', Logging.LOG_TRACE)
            self.start_inspect()

        self.client.set_on_connected(connected_)
        self.client_back.set_on_connected(back_connected_)
        self.do_start_hsdev()

    def do_start_hsdev(self):
        self.connect_clients()
        return True

    def stop_hsdev(self):
        self.do_stop_hsdev()
        self.disconnect_clients()

    def do_stop_hsdev(self):
        pass

    def connect_clients(self):
        self.client.connect()
        self.client_back.connect()

    def disconnect_clients(self):
        self.client.close()
        self.client_back.close()
        self.client = None
        self.client_back = None

    def is_connected(self):
        return self.client.is_connected()

    def on_hsdev_enabled(self, key, value):
        if key == 'enable_hsdev':
            if value:
                Logging.log("starting hsdev", Logging.LOG_INFO)
                self.start_hsdev()
            else:
                Logging.log("stopping hsdev", Logging.LOG_INFO)
                self.stop_hsdev()

    def on_inspect_modules_changed(self, key, value):
        if key == 'inspect_modules' and value:
            self.mark_all_files()

    def run(self):
        if not Settings.PLUGIN.enable_hsdev:
            return

        Settings.PLUGIN.add_change_callback('enable_hsdev', self.on_hsdev_enabled)
        Settings.PLUGIN.add_change_callback('inspect_modules', self.on_inspect_modules_changed)

        self.start_hsdev()

        while True:
            if Settings.PLUGIN.enable_hsdev and not self.client.ping():
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
                self.client_back.docs(files=files_to_reinspect)

            self.reinspect_event.wait(HsDevLocalAgent.sleep_timeout)
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
            self.client_back.scan(cabal=(cabal == 'cabal'),
                                  sandboxes=[] if cabal == 'cabal' else [cabal],
                                  on_notify=ScanStatus(smgr),
                                  wait=True,
                                  docs=Settings.PLUGIN.enable_hdocs)

    @use_hsdev()
    @use_inspect_modules
    def inspect(self, paths, projects, files):
        if paths or projects or files:
            with Common.status_message_process('Inspecting', priority=1) as smgr:
                self.client_back.scan(paths=paths,
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
            self.client_back.scan(paths=[path],
                                  on_notify=ScanStatus(smgr),
                                  wait=True,
                                  ghc=Settings.PLUGIN.ghc_opts,
                                  docs=Settings.PLUGIN.enable_hdocs)

    @use_hsdev()
    @use_inspect_modules
    def inspect_project(self, cabal_dir):
        (project_name, _) = Common.get_cabal_in_dir(cabal_dir)

        with Common.status_message_process('Inspecting project {0}'.format(project_name), priority=1) as smgr:
            self.client_back.scan(projects=[cabal_dir],
                                  on_notify=ScanStatus(smgr),
                                  wait=True,
                                  docs=Settings.PLUGIN.enable_hdocs)

    @use_hsdev()
    @use_inspect_modules
    def inspect_files(self, filenames):
        with Common.status_message_process('Inspecting files', priority=1) as smgr:
            self.client_back.scan(files=filenames,
                                  on_notify=ScanStatus(smgr),
                                  wait=True,
                                  ghc=Settings.PLUGIN.ghc_opts,
                                  docs=Settings.PLUGIN.enable_hdocs)


class HsDevRemoteAgent(HsDevAgent):
    def __init__(self, host, port):
        super().__init__(host, port)


class HsDevLocalAgent(HsDevAgent):
    """Local `hsdev` agent.

    This object is a container for the `hsdev` server process and maintains connections to two `hsdev` clients:
    the `client` connection for commands and the `client_back` for background tasks. `HsDevLocalAgent` also
    automatically reinspects files/paths/etc. when they marked as dirty.
    """

    sleep_timeout = 60.0  # agent sleeping timeout

    hsdev_not_found = ["SublimeHaskell: hsdev executable couldn't be found!",
                       "It's used in most features of SublimeHaskell",
                       "Check if it's installed and in PATH",
                       "If it's not installed, run 'cabal install hsdev' to install hsdev",
                       "You may also want to adjust 'add_to_PATH' setting",
                       "",
                       "To supress this message and disable hsdev set 'enable_hsdev' to false"
                      ]

    def __init__(self, port):
        super().__init__("localhost", port)
        self.hsdev_process = None

        hsdev_ver = hsdev_version()
        if hsdev_ver is None:
            Common.output_error_async(sublime.active_window(), "\n".join(HsDevLocalAgent.hsdev_not_found))
        elif not check_version(hsdev_ver, HSDEV_MIN_VER):
            Common.output_error_async(sublime.active_window(), "\n".join(HsDevLocalAgent.hsdev_wrong_version(hsdev_ver)))
        else:
            hsdev_log_settings = Settings.PLUGIN.hsdev_log_config
            if patch_simple_log(hsdev_ver):
                hsdev_log_settings = Settings.PLUGIN.hsdev_log_level

            self.hsdev_process = HsDevProcess(port, hsdev_ver,
                                              cache=os.path.join(Common.sublime_haskell_cache_path(), 'hsdev'),
                                              log_file=os.path.join(Common.sublime_haskell_cache_path(), 'hsdev', 'hsdev.log'),
                                              log_config=hsdev_log_settings)

    def do_start_hsdev(self):
        if self.hsdev_process is not None:
            def start_():
                Logging.log('hsdev process started', Logging.LOG_TRACE)
                self.connect_clients()

            def exit_():
                Logging.log('hsdev process exited', Logging.LOG_TRACE)
                self.disconnect_clients()

            def connected_():
                Logging.log('hsdev agent: primary connection to hsdev established', Logging.LOG_DEBUG)
                self.client.link()

            self.hsdev_process.on_start = start_
            self.hsdev_process.on_exit = exit_
            self.client.set_on_connected(connected_)

            self.hsdev_process.start()
            self.hsdev_process.create()

        return self.hsdev_process is not None


    def do_stop_hsdev(self):
        Logging.log('local hsdev stopping...', Logging.LOG_INFO)
        self.client.exit()
        self.hsdev_process.stop()


    @staticmethod
    def hsdev_wrong_version(version_str):
        return ["SublimeHaskell: hsdev version is incorrect: {0}".format(show_version(version_str)),
                "Required version: >= {0}".format(show_version(HSDEV_MIN_VER)),
                "Update it by running 'cabal update' and 'cabal install hsdev'",
                "",
                "To supress this message and disable hsdev set 'enable_hsdev' to false"
               ]

class HsDevWindowCommand(Common.SublimeHaskellWindowCommand):
    def is_enabled(self):
        return Settings.PLUGIN.enable_hsdev and \
               agent_connected() and \
               Common.SublimeHaskellWindowCommand.is_enabled(self)

    def is_visible(self):
        return Settings.PLUGIN.enable_hsdev and Common.SublimeHaskellWindowCommand.is_visible(self)


class HsDevTextCommand(Common.SublimeHaskellTextCommand):
    def is_enabled(self):
        return Settings.PLUGIN.enable_hsdev and \
               agent_connected() and \
               Common.SublimeHaskellTextCommand.is_enabled(self)

    def is_visible(self):
        return Settings.PLUGIN.enable_hsdev and Common.SublimeHaskellTextCommand.is_visible(self)


def start_agent():
    global agent
    global client
    global client_back

    if agent is None:
        Logging.log('starting agent', Logging.LOG_TRACE)

        if Settings.PLUGIN.hsdev_local_process:
            agent = HsDevLocalAgent(Settings.PLUGIN.hsdev_port)
        else:
            agent = HsDevRemoteAgent(Settings.PLUGIN.hsdev_host, Settings.PLUGIN.hsdev_port)
        agent.start()
        client = agent.client
        client_back = agent.client_back

def stop_agent():
    global agent
    global client
    global client_back

    if agent is not None:
        agent.stop_hsdev()
        agent = None
        client = None
        client_back = None

