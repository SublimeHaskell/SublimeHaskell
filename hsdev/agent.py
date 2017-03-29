# -*- coding: UTF-8 -*-

import io
import json
import os
import re
import sys
import socket
import threading
import time
import traceback

import sublime

import SublimeHaskell.sublime_haskell_common as Common
import SublimeHaskell.hsdev.client as HsDevClient
import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.hsdev.decorators as HsDecorator
import SublimeHaskell.hsdev.result_parse as ResultParse
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.worker as Worker


def hsdev_version():
    retval = None
    try:
        exit_code, out, err = ProcHelper.ProcHelper.run_process(['hsdev', 'version'])
        if exit_code == 0:
            m = re.match(r'(?P<major>\d+)\.(?P<minor>\d+)\.(?P<revision>\d+)\.(?P<build>\d+)', out)
            if m:
                major = int(m.group('major'))
                minor = int(m.group('minor'))
                revision = int(m.group('revision'))
                build = int(m.group('build'))
                retval = [major, minor, revision, build]
    except:
        Logging.log('Could not get hsdev version, see console window traceback', Logging.LOG_ERROR)
        print(traceback.format_exc())
    finally:
        return retval


def show_version(ver):
    return '.'.join(map(lambda i: str(i), ver))


def check_version(ver, minimal, maximal):
    return ver is not None and ver >= minimal and (not maximal or ver < maximal)


def format_error_details(ds):
    return ', '.join(['{}: {}'.format(k, v) for k, v in ds.items()])


def wait_result(fn, *args, **kwargs):
    wait_receive = threading.Event()
    x = {'result': None}

    on_resp = kwargs.get('on_response')
    on_err = kwargs.get('on_error')

    def wait_response(r):
        x['result'] = r
        if on_resp:
            on_resp(r)
        wait_receive.set()

    def wait_error(e, ds):
        Logging.log('hsdev call fails with: {0}, {1}'.format(e, format_error_details(ds)))
        if on_err:
            on_err(e, ds)
        wait_receive.set()

    tm = kwargs.pop('timeout', 0.1)

    kwargs['on_response'] = wait_response
    kwargs['on_error'] = wait_error

    fn(*args, **kwargs)

    wait_receive.wait(tm)
    return x['result']


# hsdev server process with auto-restart
class HsDevProcess(threading.Thread):
    def __init__(self, port=4567, cache=None, log_file=None, log_config=None):
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

    def run(self):
        while True:
            self.create_event.wait()
            self.create_event.clear()
            while not self.stop_event.is_set():
                self.process = HsDevClient.HsDev.create_server(port=self.port,
                                                               cache=self.cache,
                                                               log_file=self.log_file,
                                                               log_config=self.log_config)
                if not self.process:
                    Logging.log('failed to create hsdev process', Logging.LOG_ERROR)
                    self.stop_event.set()
                else:
                    self.drain_stdout = OutputCollector.DescriptorDrain('hsdev stdout', self.process.stdout)
                    self.drain_stderr = OutputCollector.DescriptorDrain('hsdev stderr', self.process.stderr)
                    self.drain_stdout.start()
                    self.drain_stderr.start()
                    HsCallback.call_callback(self.on_start, name='HsDevProcess.on_start')
                self.process.wait()
                if self.drain_stdout:
                    self.drain_stdout.stop()
                if self.drain_stderr:
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
class scan_status(object):
    def __init__(self, status_message):
        self.status_message = status_message

    def __call__(self, msg):
        statuses = []
        for m in msg:
            p = m['progress']
            statuses.append('{0} ({1}/{2})'.format(m['name'], p['current'], p['total']) if p else m['name'])
        self.status_message.change_message('Inspecting {0}'.format(' / '.join(statuses)))


# Set reinspect event
def dirty(fn):
    def wrapped(self, *args, **kwargs):
        if not hasattr(self, 'dirty_lock'):
            self.dirty_lock = threading.Lock()
        acquired = self.dirty_lock.acquire(blocking=False)
        try:
            return fn(self, *args, **kwargs)
        finally:
            if acquired:
                self.dirty_lock.release()
                self.reinspect_event.set()
    return wrapped


def use_inspect_modules(fn):
    def wrapped(self, *args, **kwargs):
        if Settings.get_setting_async('inspect_modules'):
            return fn(self, *args, **kwargs)
    return wrapped


def agent_connected():
    return agent.is_connected()


# Return default value if hsdev is not enabled/connected
def use_hsdev(def_val=None):
    def wrap(fn):
        def wrapped(*args, **kwargs):
            if Settings.get_setting_async('enable_hsdev') and agent_connected():
                return fn(*args, **kwargs)
            else:
                return def_val
        return wrapped
    return wrap


# hsdev agent
# holds hsdev server process and two clients: for commands and for background tasks
# also automatically reinspects files/paths/etc. when they marked as dirty
class HsDevAgent(threading.Thread):
    sleep_timeout = 60.0  # agent sleeping timeout
    min_ver = [0, 2, 0, 0]  # minimal hsdev version
    max_ver = [0, 2, 3, 0]  # maximal hsdev version

    def __init__(self):
        super().__init__()
        self.daemon = True
        self.cabal_to_load = LockedObject.LockedObject([])
        self.dirty_files = LockedObject.LockedObject([])
        self.dirty_paths = LockedObject.LockedObject([])
        self.hsdev_process = HsDevProcess(cache=os.path.join(Common.sublime_haskell_cache_path(), 'hsdev'),
                                          log_file=os.path.join(Common.sublime_haskell_cache_path(), 'hsdev', 'hsdev.log'),
                                          log_config=Settings.get_setting_async('hsdev_log_config'))
        self.client = HsDevClient.HsDev()
        self.client_back = HsDevClient.HsDev()

        self.reinspect_event = threading.Event()

    def is_connected(self):
        return self.client.is_connected()

    def start_hsdev(self, start_server=True):
        hsdev_ver = hsdev_version()
        if hsdev_ver is None:
            Common.output_error_async(sublime.active_window(), "\n".join([
                "SublimeHaskell: hsdev executable couldn't be found!",
                "It's used in most features of SublimeHaskell",
                "Check if it's installed and in PATH",
                "If it's not installed, run 'cabal install hsdev' to install hsdev",
                "You may also want to adjust 'add_to_PATH' setting",
                "",
                "To supress this message and disable hsdev set 'enable_hsdev' to false"]))
        elif not check_version(hsdev_ver, HsDevAgent.min_ver, HsDevAgent.max_ver):
            Common.output_error_async(sublime.active_window(), "\n".join([
                "SublimeHaskell: hsdev version is incorrect: {0}".format(show_version(hsdev_ver)),
                "Required version: >= {0} and < {1}".format(show_version(HsDevAgent.min_ver), show_version(HsDevAgent.max_ver)),
                "Update it by running 'cabal update' and 'cabal install hsdev'",
                "",
                "To supress this message and disable hsdev set 'enable_hsdev' to false"]))
        else:
            def start_():
                Logging.log('hsdev process started', Logging.LOG_TRACE)
                self.client.connect_async()
                self.client_back.connect_async()

            def exit_():
                Logging.log('hsdev process exited', Logging.LOG_TRACE)
                self.client.close()
                self.client_back.close()

            def connected_():
                Logging.log('hsdev agent: connected to hsdev', Logging.LOG_TRACE)
                self.client.link()

            def back_connected_():
                Logging.log('hsdev agent: connected to hsdev', Logging.LOG_TRACE)
                self.start_inspect()

            self.client.on_connected = connected_
            self.client_back.on_connected = back_connected_

            self.hsdev_process.on_start = start_
            self.hsdev_process.on_exit = exit_

            self.hsdev_process.start()
            self.hsdev_process.create()

    def stop_hsdev(self):
        self.client.close()
        self.client_back.close()

    def on_hsdev_enabled(self, key, value):
        if key == 'enable_hsdev':
            if value:
                Logging.log("starting hsdev", Logging.LOG_INFO)
                self.hsdev_process.create()
            else:
                Logging.log("stopping hsdev", Logging.LOG_INFO)
                self.hsdev_process.stop()
                self.client.close()
                self.client_back.close()

    def on_inspect_modules_changed(self, key, value):
        if key == 'inspect_modules':
            if value:
                self.mark_all_files()

    def run(self):
        Settings.subscribe_setting('enable_hsdev', self.on_hsdev_enabled)
        Settings.subscribe_setting('inspect_modules', self.on_inspect_modules_changed)

        if Settings.get_setting_async('enable_hsdev'):
            self.start_hsdev()

        while True:
            if Settings.get_setting_async('enable_hsdev') and not self.client.ping():
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
                for f in files_to_reinspect:
                    d = Common.get_cabal_project_dir_of_file(f)
                    if d is not None:
                        projects.append(d)
                    else:
                        files.append(f)

            projects = list(set(projects))
            files = list(set(files))

            try:
                self.inspect(paths=scan_paths, projects=projects, files=files)
            except Exception as e:
                Logging.log('HsDevAgent inspect exception: {0}'.format(e))

            load_cabal = []
            with self.cabal_to_load as cabal_to_load:
                load_cabal = cabal_to_load[:]
                cabal_to_load[:] = []

            for c in load_cabal:
                Worker.run_async('inspect cabal {0}'.format(c), self.inspect_cabal, c)

            if files_to_reinspect:
                if Settings.get_setting_async('enable_hdocs'):
                    self.client_back.docs(files=files_to_reinspect)
            self.reinspect_event.wait(HsDevAgent.sleep_timeout)
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
        window = sublime.active_window()
        with self.dirty_files as dirty_files:
            dirty_files.extend(list(filter(lambda f: f and f.endswith('.hs'), [v.file_name() for v in window.views()])))
        with self.dirty_paths as dirty_paths:
            dirty_paths.extend(window.folders())

    @dirty
    @use_inspect_modules
    def mark_file_dirty(self, filename):
        if filename is None:
            return
        with self.dirty_files as dirty_files:
            dirty_files.append(filename)

    @dirty
    def mark_cabal(self, cabal_name=None):
        with self.cabal_to_load as cabal_to_load:
            cabal_to_load.append(cabal_name or 'cabal')

    @use_hsdev()
    def inspect_cabal(self, cabal=None):
        try:
            with Common.status_message_process('Inspecting {0}'.format(cabal or 'cabal'), priority=1) as smgr:
                self.client_back.scan(cabal=(cabal == 'cabal'),
                                      sandboxes=[] if cabal == 'cabal' else [cabal],
                                      on_notify=scan_status(smgr),
                                      wait=True,
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except:
            Logging.log('loading standard modules info for {0} failed:'.format(cabal or 'cabal'),
                        Logging.LOG_ERROR)
            print(traceback.format_exc())

    @use_hsdev()
    @use_inspect_modules
    def inspect(self, paths, projects, files):
        if paths or projects or files:
            try:
                with Common.status_message_process('Inspecting', priority=1) as smgr:
                    self.client_back.scan(paths=paths,
                                          projects=projects,
                                          files=files,
                                          on_notify=scan_status(smgr),
                                          wait=True,
                                          ghc=Settings.get_setting_async('ghc_opts'),
                                          docs=Settings.get_setting_async('enable_hdocs'))
            except:
                Logging.log('Inspection failed, see console window traceback', Logging.LOG_ERROR)
                print(traceback.format_exc())

    @use_hsdev()
    @use_inspect_modules
    def inspect_path(self, path):
        try:
            with Common.status_message_process('Inspecting path {0}'.format(path), priority=1) as smgr:
                self.client_back.scan(paths=[path],
                                      on_notify=scan_status(smgr),
                                      wait=True,
                                      ghc=Settings.get_setting_async('ghc_opts'),
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except:
            Logging.log('Inspecting path {0} failed, see console window traceback'.format(path), Logging.LOG_ERROR)
            print(traceback.format_exc())

    @use_hsdev()
    @use_inspect_modules
    def inspect_project(self, cabal_dir):
        (project_name, _) = Common.get_cabal_in_dir(cabal_dir)

        try:
            with Common.status_message_process('Inspecting project {0}'.format(project_name), priority=1) as smgr:
                self.client_back.scan(projects=[cabal_dir],
                                      on_notify=scan_status(smgr),
                                      wait=True,
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except:
            Logging.log('Inspecting project {0} failed, see console window traceback'.format(cabal_dir), Logging.LOG_ERROR)
            print(traceback.format_exc())

    @use_hsdev()
    @use_inspect_modules
    def inspect_files(self, filenames):
        try:
            with Common.status_message_process('Inspecting files', priority=1) as smgr:
                self.client_back.scan(files=filenames,
                                      on_notify=scan_status(smgr),
                                      wait=True,
                                      ghc=Settings.get_setting_async('ghc_opts'),
                                      docs=Settings.get_setting_async('enable_hdocs'))
        except:
            Logging.log('Inspecting files failed, see console window traceback', Logging.LOG_ERROR)
            print(traceback.format_exc())


class HsDevWindowCommand(Common.SublimeHaskellWindowCommand):
    def is_enabled(self):
        return Settings.get_setting_async('enable_hsdev') and \
               agent_connected() and \
               Common.SublimeHaskellWindowCommand.is_enabled(self)

    def is_visible(self):
        return Settings.get_setting_async('enable_hsdev') and Common.SublimeHaskellWindowCommand.is_visible(self)


class HsDevTextCommand(Common.SublimeHaskellTextCommand):
    def is_enabled(self):
        return Settings.get_setting_async('enable_hsdev') and \
               agent_connected() and \
               Common.SublimeHaskellTextCommand.is_enabled(self)

    def is_visible(self):
        return Settings.get_setting_async('enable_hsdev') and Common.SublimeHaskellTextCommand.is_visible(self)


def start_agent():
    global agent
    global client
    global client_back

    if agent is None:
        Logging.log('starting agent', Logging.LOG_TRACE)

        agent = HsDevAgent()
        agent.start()
        client = agent.client
        client_back = agent.client_back
