"""
The `hsdev` backend.
"""

from functools import reduce
import io
import os
import os.path
import re
import subprocess
import threading

import sublime

import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.hsdev.client as HsDevClient
import SublimeHaskell.hsdev.result_parse as ResultParse
import SublimeHaskell.internals.backend as Backend
import SublimeHaskell.internals.logging as Logging
import SublimeHaskell.internals.output_collector as OutputCollector
import SublimeHaskell.internals.proc_helper as ProcHelper
import SublimeHaskell.internals.settings as Settings
import SublimeHaskell.internals.which as Which
import SublimeHaskell.internals.utils as Utils
import SublimeHaskell.sublime_haskell_common as Common


def result_identity(resp):
    '''Identity function for results
    '''
    return resp


class HsDevBackend(Backend.HaskellBackend):
    """This class encapsulates all of the functions that interact with the `hsdev` backend.
    """

    HSDEV_DEFAULT_PORT = 4567
    HSDEV_DEFAULT_HOST = 'localhost'
    HSDEV_MIN_VER = [0, 2, 0, 0]  # minimum hsdev version
    HSDEV_MAX_VER = [0, 2, 4, 0]  # maximum hsdev version
    HSDEV_CALL_TIMEOUT = 300.0 # second timeout for synchronous requests

    def __init__(self, backend_mgr, local=True, port=HSDEV_DEFAULT_PORT, host=HSDEV_DEFAULT_HOST, **kwargs):
        super().__init__(backend_mgr)

        # Sanity checking:
        exec_with = kwargs.get('exec-with')
        install_dir = kwargs.get('install-dir')
        if exec_with is not None and install_dir is None:
            sublime.error_message('\n'.join(['\'exec_with\' requires an \'install_dir\'.',
                                             '',
                                             'Please check your \'backends\' configuration and retry.']))
            raise RuntimeError('\'exec_with\' requires an \'install_dir\'.')
        elif exec_with is not None and exec_with not in ['stack', 'cabal']:
            sublime.error_message('\n'.join(['Invalid backend \'exec_with\': {0}'.format(exec_with),
                                             '',
                                             'Valid values are "cabal" or "stack".',
                                             'Please check your \'backends\' configuration and retry.']))
            raise RuntimeError('Invalid backend \'exec_with\': {0}'.format(exec_with))

        # Local hsdev server process and params
        self.is_local_hsdev = local
        self.hsdev_process = None
        self.cache = os.path.join(Common.sublime_haskell_cache_path(), 'hsdev')
        self.log_file = os.path.join(Common.sublime_haskell_cache_path(), 'hsdev', 'hsdev.log')
        self.exec_with = exec_with
        self.install_dir = Utils.normalize_path(install_dir) if install_dir is not None else None
        # Keep track of the hsdev version early. Needed to patch command line arguments later.
        hsdev_path = Which.which('hsdev', ProcHelper.ProcHelper.get_extended_path())
        self.version = HsDevBackend.hsdev_version() if hsdev_path is not None else [0, 0, 0, 0]

        self.drain_stdout = None
        self.drain_stderr = None
        # Connection params
        self.port = port
        self.hostname = host
        if self.is_local_hsdev:
            self.hostname = self.HSDEV_DEFAULT_HOST
        self.client = None

    @staticmethod
    def backend_name():
        return 'hsdev'

    @staticmethod
    def is_available():
        # Yes, this is slightly redundant because eventually __init__ does the same thing for a class
        # instance.
        hsdev_path = Which.which('hsdev', ProcHelper.ProcHelper.get_extended_path())
        hsdev_ver = HsDevBackend.hsdev_version() if hsdev_path is not None else [0, 0, 0, 0]
        Logging.log('hsdev version: {0}'.format('.'.join(map(str, hsdev_ver))), Logging.LOG_INFO)
        return hsdev_path is not None and \
               (hsdev_ver >= HsDevBackend.HSDEV_MIN_VER and hsdev_ver <= HsDevBackend.HSDEV_MAX_VER)

    def start_backend(self):
        retval = True
        if self.is_local_hsdev:
            Logging.log('Starting local \'hsdev\'server', Logging.LOG_INFO)

            use_log_level = self.version >= [0, 2, 3, 2]
            log_config = Settings.PLUGIN.hsdev_log_config
            log_level = Settings.PLUGIN.hsdev_log_level

            cmd = self.concat_args([(self.exec_with is not None and self.exec_with == 'cabal', ['cabal', 'exec']),
                                    (self.exec_with is not None and self.exec_with == 'stack', ['stack', 'exec']),
                                    (True, ["hsdev"]),
                                    (self.exec_with is not None, ['--']),
                                    (True, ["run"]),
                                    (self.port, ["--port", str(self.port)]),
                                    (self.cache, ["--cache", self.cache]),
                                    (self.log_file, ["--log", self.log_file]),
                                    (not use_log_level and log_config, ["--log-config", log_config]),
                                    (use_log_level, ["--log-level", log_level])])

            Logging.log('hsdev command: {0}'.format(cmd), Logging.LOG_DEBUG)

            proc_args = {}
            if self.install_dir is not None:
                proc_args['cwd'] = self.install_dir

            hsdev_proc = ProcHelper.ProcHelper(cmd, **proc_args)
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
                    if hsdev_proc.process_err is not None:
                        Logging.log('Possible reason for timeout: {0}'.format(hsdev_proc.process_err))
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
        self.client = HsDevClient.HsDevClient(self.backend_mgr)
        if self.client.connect(self.hostname, self.port):
            # For a local hsdev server that we started, send the link command so that it exits when we exit.
            if self.is_local_hsdev:
                self.link()
        else:
            Logging.log('Connections to \'hsdev\' server unsuccessful, see tracebacks to diagnose.', Logging.LOG_ERROR)
            retval = False
        return retval

    def disconnect_backend(self):
        self.exit()
        self.client.close()

    def stop_backend(self):
        if self.is_local_hsdev:
            try:
                self.hsdev_process.process.wait(90.0)
            except subprocess.TimeoutExpired:
                sublime.message_dialog('\n'.join(['Time out waiting for \'hsdev\' process to terminate.',
                                                  '',
                                                  'You may have to kill this process manually from a terminal or',
                                                  'console window\'s command line.']))

    def is_live_backend(self):
        return self.client.is_connected()

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # File/project tracking functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def add_project_file(self, filename, project, project_dir):
        super().add_project_file(filename, project, project_dir)

    def remove_project_file(self, filename):
        super().remove_project_file(filename)

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Utility functions used to implement the API:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

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

    def files_and_contents(self, files, contents):
        retval = [{'file': f, 'contents': None} for f in files] if files is not None else []
        retval.extend([{'file': f, 'contents': cts} for f, cts in contents.items()] if contents is not None else [])
        return  retval


    def hsdev_command(self, name, opts, on_result, async=False, timeout=HSDEV_CALL_TIMEOUT, is_list=False,
                      on_response=None, on_notify=None, on_error=None, on_result_part=None, split_result=None):
        if split_result is None:
            split_res = on_result_part is not None

        if is_list and split_res:
            result = []

            def hsdev_command_notify(reply):
                if 'result-part' in reply:
                    notify_result = on_result([reply['result-part']])[0]
                    HsCallback.call_callback(on_result_part, notify_result)
                    result.append(notify_result)
                else:
                    HsCallback.call_callback(on_notify, reply)

            # FIXME: Is this option still used?
            opts.update({'split-result': None})
            resp = self.client.call(name,
                                    opts,
                                    on_response=on_response,
                                    on_notify=hsdev_command_notify,
                                    on_error=on_error,
                                    wait=not async,
                                    timeout=timeout)

            return result if not async else resp

        else:
            def process_response(resp):
                on_response(on_result(resp))

            resp = self.client.call(name,
                                    opts,
                                    on_response=process_response if on_response else None,
                                    on_notify=on_notify,
                                    on_error=on_error,
                                    wait=not async,
                                    timeout=timeout)

            return on_result(resp) if not async else resp

    def command(self, name, opts, on_result=result_identity, timeout=HSDEV_CALL_TIMEOUT, on_response=None,
                on_notify=None, on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=False, timeout=timeout, is_list=False,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)


    def async_command(self, name, opts, on_result=result_identity, on_response=None, on_notify=None,
                      on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=True, timeout=None, is_list=False,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)


    def list_command(self, name, opts, on_result=result_identity, timeout=HSDEV_CALL_TIMEOUT, on_response=None,
                     on_notify=None, on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=False, timeout=timeout, is_list=True,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)

    def async_list_command(self, name, opts, on_result=result_identity, on_response=None,
                           on_notify=None, on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=True, timeout=None, is_list=True,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API implementation:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def link(self, hold=False):
        return self.command('link', {'hold': hold})

    def ping(self):
        return self.command('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'))

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False, wait_complete=False, **backend_args):
        action = self.command if wait_complete else self.async_command
        return action('scan', {'projects': projects or [],
                               'cabal': cabal,
                               'sandboxes': sandboxes or [],
                               'files': self.files_and_contents(files, contents),
                               'paths': paths or [],
                               'ghc-opts': ghc or [],
                               'docs': docs,
                               'infer': infer},
                      **backend_args)

    def docs(self, projects=None, files=None, modules=None, **backend_args):
        return self.async_command('docs', {'projects': projects or [],
                                           'files': files or [],
                                           'modules': modules or []},
                                  **backend_args)

    def infer(self, projects=None, files=None, modules=None, **backend_args):
        return self.async_command('infer', {'projects': projects or [],
                                            'files': files or [],
                                            'modules': modules or []},
                                  **backend_args)

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None, **backend_args):
        return self.async_list_command('remove', {'projects': projects or [],
                                                  'cabal': cabal,
                                                  'sandboxes': sandboxes or [],
                                                  'files': files or [],
                                                  'packages': packages or []},
                                       **backend_args)

    def remove_all(self, **backend_args):
        return self.command('remove-all', {}, **backend_args)

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False, **backend_args):
        filters = []
        if project:
            filters.append({'project': project})
        if file:
            filters.append({'file': file})
        if module:
            filters.append({'module': module})
        if deps:
            filters.append({'deps': deps})
        if sandbox:
            filters.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            filters.append({'cabal': 'cabal'})
        if symdb:
            filters.append({'db': ResultParse.encode_package_db(symdb)})
        if package:
            filters.append({'package': package})
        if source:
            filters.append('sourced')
        if standalone:
            filters.append('standalone')

        return self.list_command('modules', {'filters': filters}, ResultParse.parse_modules_brief, **backend_args)

    def list_packages(self, **backend_args):
        return self.list_command('packages', {}, **backend_args)

    def list_projects(self, **backend_args):
        return self.list_command('projects', {}, **backend_args)

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False, **backend_args):
        # search_type is one of: exact, prefix, infix, suffix, regex
        query = {'input': lookup, 'type': search_type}

        filters = []
        if project:
            filters.append({'project': project})
        if file:
            filters.append({'file': file})
        if module:
            filters.append({'module': module})
        if deps:
            filters.append({'deps': deps})
        if sandbox:
            filters.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            filters.append({'cabal': 'cabal'})
        if symdb:
            filters.append({'db': ResultParse.encode_package_db(symdb)})
        if package:
            filters.append({'package': package})
        if source:
            filters.append('sourced')
        if standalone:
            filters.append('standalone')

        return self.list_command('symbol', {'query': query, 'filters': filters, 'locals': local_names},
                                 ResultParse.parse_decls, **backend_args)

    def module(self, _projectname, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None,
               sandbox=None, cabal=False, symdb=None, package=None, source=False, standalone=False, **backend_args):
        query = {'input': lookup, 'type': search_type}

        filters = []
        if project:
            filters.append({'project': project})
        if file:
            filters.append({'file': file})
        if module:
            filters.append({'module': module})
        if deps:
            filters.append({'deps': deps})
        if sandbox:
            filters.append({'cabal': {'sandbox': sandbox}})
        if cabal:
            filters.append({'cabal': 'cabal'})
        if symdb:
            filters.append({'db': ResultParse.encode_package_db(symdb)})
        if package:
            filters.append({'package': package})
        if source:
            filters.append('sourced')
        if standalone:
            filters.append('standalone')

        return self.command('module', {'query': query, 'filters': filters}, ResultParse.parse_modules, **backend_args)

    def resolve(self, file, exports=False, **backend_args):
        return self.command('resolve', {'file': file, 'exports': exports}, ResultParse.parse_module, **backend_args)

    def project(self, project=None, path=None, **backend_args):
        return self.command('project', {'name': project} if project else {'path': path}, **backend_args)

    def sandbox(self, path, **backend_args):
        return self.command('sandbox', {'path': path}, **backend_args)

    def lookup(self, name, file, **backend_args):
        return self.list_command('lookup', {'name': name, 'file': file}, ResultParse.parse_decls, **backend_args)

    def whois(self, name, file, **backend_args):
        return self.list_command('whois', {'name': name, 'file': file}, ResultParse.parse_declarations, **backend_args)

    def scope_modules(self, _projcname, file, lookup='', search_type='prefix', **backend_args):
        return self.list_command('scope modules', {'query': {'input': lookup, 'type': search_type}, 'file': file},
                                 ResultParse.parse_modules_brief, **backend_args)

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        return self.list_command('scope',
                                 {'query': {'input': lookup,
                                            'type': search_type
                                           },
                                  'global': global_scope,
                                  'file': file
                                 }, ResultParse.parse_declarations, **backend_args)

    def complete(self, lookup, file, wide=False, **backend_args):
        return self.list_command('complete', {'prefix': lookup, 'wide': wide, 'file': file},
                                 ResultParse.parse_declarations, **backend_args)

    def hayoo(self, query, page=None, pages=None, **backend_args):
        return self.list_command('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1},
                                 ResultParse.parse_decls, **backend_args)

    def cabal_list(self, packages, **backend_args):
        return self.list_command('cabal list', {'packages': packages},
                                 lambda r: [ResultParse.parse_cabal_package(s) for s in r] if r else None,
                                 **backend_args)

    def lint(self, files=None, contents=None, hlint=None, wait_complete=False, **backend_args):
        action = self.list_command if wait_complete else self.async_list_command
        return action('lint', {'files': self.files_and_contents(files, contents),
                               'hlint-opts': hlint or []},
                      **backend_args)

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        action = self.list_command if wait_complete else self.async_list_command
        return action('check', {'files': self.files_and_contents(files, contents),
                                'ghc-opts': ghc or []},
                      **backend_args)

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=False, **backend_args):
        action = self.list_command if wait_complete else self.async_list_command
        return action('check-lint', {'files': self.files_and_contents(files, contents),
                                     'ghc-opts': ghc or [],
                                     'hlint-opts': hlint or []},
                      **backend_args)

    def types(self, _projectname, file, _modulename, _line, _column, ghc_flags=None, contents=None, **backend_args):
        return self.list_command('types', {'files': self.files_and_contents(file, contents),
                                           'ghc-opts': ghc_flags or []},
                                 **backend_args)

    def langs(self, _projectname, **backend_args):
        return self.command('langs', {}, **backend_args)

    def flags(self, _projectname, **backend_args):
        return self.command('flags', {}, **backend_args)

    def autofix_show(self, messages, **backend_args):
        return self.async_list_command('autofix show', {'messages': messages}, ResultParse.parse_corrections, **backend_args)

    def autofix_fix(self, messages, rest=None, pure=False, **backend_args):
        return self.autofix_fix('autofix fix', {'messages': messages, 'rest': rest or [], 'pure': pure},
                                ResultParse.parse_corrections, **backend_args)

    def ghc_eval(self, exprs, file=None, source=None, **backend_args):
        the_file = None
        if file is not None:
            the_file = {'file': the_file, 'contents': source}
        return self.list_command('ghc eval', {'exprs': exprs, 'file': the_file}, **backend_args)

    def exit(self):
        return self.command('exit', {})

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
            Logging.log('hsdev initial: {0}'.format(srvout), Logging.LOG_DEBUG)
            if srvout != '':
                start_confirm = re.search(r'[Ss]erver started at port (?P<port>\d+)$', srvout)
                if start_confirm:
                    self.hsdev_port = int(start_confirm.group('port'))
                    Logging.log('hsdev initial: \'hsdev\' server started at port {0}'.format(self.hsdev_port))
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
