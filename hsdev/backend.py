"""
The `hsdev` backend.
"""

from functools import reduce
import io
import os
import re
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
import SublimeHaskell.sublime_haskell_common as Common


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
        self.main_client = HsDevClient.HsDevClient()
        self.aux_client = HsDevClient.HsDevClient()
        if self.main_client.connect(self.hostname, self.port) and self.aux_client.connect(self.hostname, self.port):
            # For a local hsdev server that we started, send the link command so that it exits when we exit.
            if self.is_local_hsdev:
                self.link()
            # Start the inspection process...
            # FIXME: Settings.PLUGIN.add_change_callback('inspect_modules', self.on_inspect_modules_changed)
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

    def files_and_contents(self, files, contents):
        retval = [{'file': f, 'contents': None} for f in files] if files is not None else []
        retval.extend([{'file': f, 'contents': cts} for f, cts in contents.items()] if contents is not None else [])
        return  retval


    def hsdev_command(self, name, opts, on_result, async=False, timeout=None, is_list=False,
                      on_response=None, on_notify=None, on_error=None, on_result_part=None, split_result=None):
        if split_result is None:
            split_res = on_result_part is not None

        if is_list and split_res:
            result = []

            def inner_notify(reply):
                if 'result-part' in reply:
                    notify_result = on_result([reply['result-part']])[0]
                    HsCallback.call_callback(on_result_part, notify_result)
                    result.append(notify_result)
                else:
                    HsCallback.call_callback(on_notify, reply)

            # FIXME: Is this option still used?
            opts.update({'split-result': None})
            # FIXME: Need a connection pool
            resp = self.main_client.call(name
                                         , opts
                                         , on_response=on_response
                                         , on_notify=inner_notify
                                         , on_error=on_error
                                         , wait=not async
                                         , timeout=timeout)

            return result if not async else resp

        else:
            def processed_response(resp):
                on_response(on_result(resp))

            # FIXME: Need a connection pool
            resp = self.main_client.call(name
                                         , opts
                                         , on_response=processed_response if on_response else None
                                         , on_notify=on_notify
                                         , on_error=on_error
                                         , wait=not async
                                         , timeout=timeout)

            return on_result(resp) if not async else resp

    def command(self, name, opts, on_result=lambda r: r, on_response=None, on_notify=None,
                on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=False, timeout=1, is_list=False,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)


    def async_command(self, name, opts, on_result=lambda r: r, on_response=None, on_notify=None,
                      on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=True, timeout=None, is_list=False,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)


    def list_command(self, name, opts, on_result=lambda r: r, on_response=None, on_notify=None,
                     on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=False, timeout=1, is_list=True,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)

    def async_list_command(self, name, opts, on_result=lambda r: r, on_response=None,
                           on_notify=None, on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=True, timeout=None, is_list=True,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)

    # Commands

    def link(self, hold=False):
        resp = self.command('link', {'hold': hold})
        Logging.log('HsDevClient.link: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def ping(self):
        resp = self.command('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'))
        Logging.log('HsDevClient.ping: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False):
        resp = self.async_command('scan', {'projects': projects or [],
                                           'cabal': cabal,
                                           'sandboxes': sandboxes or [],
                                           'files': self.files_and_contents(files, contents),
                                           'paths': paths or [],
                                           'ghc-opts': ghc or [],
                                           'docs': docs,
                                           'infer': infer})
        Logging.log('HsDevClient.scan: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def docs(self, projects=None, files=None, modules=None):
        resp = self.async_command('docs', {'projects': projects or [],
                                           'files': files or [],
                                           'modules': modules or []})
        Logging.log('HsDevClient.docs: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def infer(self, projects=None, files=None, modules=None):
        resp = self.async_command('infer', {'projects': projects or [],
                                            'files': files or [],
                                            'modules': modules or []})
        Logging.log('HsDevClient.infer: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None):
        resp = self.async_list_command('remove', {'projects': projects or [],
                                                  'cabal': cabal,
                                                  'sandboxes': sandboxes or [],
                                                  'files': files or [],
                                                  'packages': packages or []})
        Logging.log('HsDevClient.remove: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def remove_all(self):
        resp = self.command('remove-all', {})
        Logging.log('HsDevClient.remove-all: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def list_modules(self, project=None, file=None, module=None, deps=None, sandbox=None, cabal=False, symdb=None, package=None,
                     source=False, standalone=False):
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

        resp = self.list_command('modules', {'filters': filters}, ResultParse.parse_modules_brief)
        Logging.log('HsDevClient.list_modules: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def list_packages(self):
        resp = self.list_command('packages', {})
        Logging.log('HsDevClient.list_packages: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def list_projects(self):
        resp = self.list_command('projects', {})
        Logging.log('HsDevClient.list_projects: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def symbol(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False, local_names=False):
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

        resp = self.list_command('symbol', {'query': query, 'filters': filters, 'locals': local_names},
                                 ResultParse.parse_decls)
        Logging.log('HsDevClient.symbol: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def module(self, lookup="", search_type='prefix', project=None, file=None, module=None, deps=None, sandbox=None,
               cabal=False, symdb=None, package=None, source=False, standalone=False):
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

        resp = self.command('module', {'query': query, 'filters': filters}, ResultParse.parse_modules)
        Logging.log('HsDevClient.module: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def resolve(self, file, exports=False):
        resp = self.command('resolve', {'file': file, 'exports': exports}, ResultParse.parse_module)
        Logging.log('HsDevClient.resolve: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def project(self, project=None, path=None):
        resp = self.command('project', {'name': project} if project else {'path': path})
        Logging.log('HsDevClient.project: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def sandbox(self, path):
        resp = self.command('sandbox', {'path': path})
        Logging.log('HsDevClient.sandbox: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def lookup(self, name, file):
        resp = self.list_command('lookup', {'name': name, 'file': file}, ResultParse.parse_decls)
        Logging.log('HsDevClient.lookup: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def whois(self, name, file):
        resp = self.list_command('whois', {'name': name, 'file': file}, ResultParse.parse_declarations)
        Logging.log('HsDevClient.whois: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def scope_modules(self, file, lookup='', search_type='prefix'):
        resp = self.list_command('scope modules', {'query': {'input': lookup, 'type': search_type}, 'file': file},
                                 ResultParse.parse_modules_brief)
        Logging.log('HsDevClient.modules: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def scope(self, file, lookup='', search_type='prefix', global_scope=False):
        resp = self.list_command('scope',
                                 {'query': {'input': lookup,
                                            'type': search_type
                                           },
                                  'global': global_scope,
                                  'file': file
                                 }, ResultParse.parse_declarations)
        Logging.log('HsDevClient.scope: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def complete(self, lookup, file, wide=False):
        resp = self.list_command('complete', {'prefix': lookup, 'wide': wide, 'file': file},
                                 ResultParse.parse_declarations)
        Logging.log('HsDevClient.complete: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def hayoo(self, query, page=None, pages=None):
        resp = self.list_command('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1},
                                 ResultParse.parse_decls)
        Logging.log('HsDevClient.hayoo: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def cabal_list(self, packages):
        resp = self.list_command('cabal list', {'packages': packages},
                                 lambda r: [ResultParse.parse_cabal_package(s) for s in r] if r else None)
        Logging.log('HsDevClient.cabal_list: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def lint(self, files=None, contents=None, hlint=None):
        resp = self.list_command('lint', {'files': self.files_and_contents(files, contents),
                                          'hlint-opts': hlint or []})
        Logging.log('HsDevClient.lint: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def check(self, files=None, contents=None, ghc=None):
        resp = self.list_command('check', {'files': self.files_and_contents(files, contents),
                                           'ghc-opts': ghc or []})
        Logging.log('HsDevClient.check: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None):
        resp = self.list_command('check-lint', {'files': self.files_and_contents(files, contents),
                                                'ghc-opts': ghc or [],
                                                'hlint-opts': hlint or []})
        Logging.log('HsDevClient.check_lint: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def types(self, files=None, contents=None, ghc=None):
        resp = self.list_command('types', {'files': self.files_and_contents(files, contents),
                                           'ghc-opts': ghc or []})
        Logging.log('HsDevClient.types: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def langs(self):
        resp = self.command('langs', {})
        Logging.log('HsDevClient.langs: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def flags(self):
        resp = self.command('flags', {})
        Logging.log('HsDevClient.flags: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def autofix_show(self, messages):
        resp = self.list_command('autofix show', {'messages': messages}, ResultParse.parse_corrections)
        Logging.log('HsDevClient.autofix_show: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def autofix_fix(self, messages, rest=None, pure=False):
        resp = self.autofix_fix('autofix fix', {'messages': messages, 'rest': rest or [], 'pure': pure},
                                ResultParse.parse_corrections)
        Logging.log('HsDevClient.autofix_fix: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def ghc_eval(self, exprs, file=None, source=None):
        the_file = None
        if file is not None:
            the_file = {'file': the_file, 'contents': source}
        resp = self.list_command('ghc eval', {'exprs': exprs, 'file': the_file})
        Logging.log('HsDevClient.ghc_eval: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

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
