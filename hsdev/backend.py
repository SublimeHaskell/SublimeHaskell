"""
The `hsdev` backend.
"""

from functools import reduce
import io
import json
import os
import os.path
import pprint
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
    HSDEV_MAX_VER = [0, 3, 0, 0]  # maximum hsdev version
    HSDEV_CALL_TIMEOUT = 300.0 # second timeout for synchronous requests (5 minutes should be enough, no?)

    def __init__(self, backend_mgr, local=True, port=HSDEV_DEFAULT_PORT, host=HSDEV_DEFAULT_HOST, **kwargs):
        super().__init__(backend_mgr)
        Logging.log('{0}.__init__({1}, {2})'.format(type(self).__name__, host, port), Logging.LOG_INFO)

        # Sanity checking:
        exec_with = kwargs.get('exec-with')
        install_dir = kwargs.get('install-dir')
        if bool(exec_with) ^ bool(install_dir):
            if install_dir is None:
                sublime.error_message('\n'.join(['\'exec_with\' requires an \'install_dir\'.',
                                                 '',
                                                 'Please check your \'backends\' configuration and retry.']))
                raise RuntimeError('\'exec_with\' requires an \'install_dir\'.')
            else:
                sublime.error_message('\n'.join(['\'install_dir\' requires an \'exec_with\'.',
                                                 '',
                                                 'Please check your \'backends\' configuration and retry.']))
                raise RuntimeError('\'install_dir\' requires an \'exec_with\'.')
        elif exec_with and exec_with not in ['stack', 'cabal', 'cabal-new-build']:
            sublime.error_message('\n'.join(['Invalid backend \'exec_with\': {0}'.format(exec_with),
                                             '',
                                             'Valid values are "cabal", "cabal-new-build" or "stack".',
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
        self.version = HsDevBackend.hsdev_version(self.exec_with, self.install_dir)

        self.drain_stdout = None
        self.drain_stderr = None
        # Connection params
        self.port = port
        self.hostname = host
        if self.is_local_hsdev:
            self.hostname = self.HSDEV_DEFAULT_HOST
        self.client = None
        self.serial_lock = threading.RLock()
        self.request_serial = 1

    @staticmethod
    def backend_name():
        return 'hsdev'

    @staticmethod
    def is_available(**kwargs):
        # Yes, this is slightly redundant because eventually __init__ does the same thing for a class
        # instance.
        exec_with = kwargs.get('exec-with')
        install_dir = kwargs.get('install-dir')
        local = kwargs.get('local', False)
        exec_install_set = not bool(exec_with) ^ bool(install_dir)
        if exec_install_set or local:
            if not exec_install_set:
                # Either exec-with or install-dir isn't set, so the corresponding configuration target is unavailable.
                return False

            hsdev_ver = HsDevBackend.hsdev_version(exec_with, install_dir)
            Logging.log('hsdev version: {0}'.format('.'.join(map(str, hsdev_ver))), Logging.LOG_INFO)
            return hsdev_ver >= HsDevBackend.HSDEV_MIN_VER and hsdev_ver < HsDevBackend.HSDEV_MAX_VER

        # Assume that a remote backend is actually available. Ultimately, we might not connect to it, but
        # it is available to us as a backend.
        return True

    def start_backend(self):
        retval = True
        if self.is_local_hsdev:
            Logging.log('Starting local \'hsdev\' server', Logging.LOG_INFO)

            use_log_level = (self.version >= [0, 2, 3, 2])
            log_config = Settings.PLUGIN.hsdev_log_config
            log_level = Settings.PLUGIN.hsdev_log_level

            cmd = self.concat_args([(True, ["hsdev"]),
                                    (True, ["run"]),
                                    (self.port, ["--port", str(self.port)]),
                                    (self.cache, ["--cache", self.cache]),
                                    (self.log_file, ["--log", self.log_file]),
                                    (not use_log_level and log_config, ["--log-config", log_config]),
                                    (use_log_level, ["--log-level", log_level])])

            hsdev_proc = ProcHelper.exec_with_wrapper(self.exec_with, self.install_dir, cmd)
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

    ## Pylint deems these two methods unncessary since all they do is call the superclass. However, I'm
    ## leaving them here just in case something more interesting has to be done in addition to calling
    ## the superclass.

    # def add_project_file(self, filename, project, project_dir):
    #     super().add_project_file(filename, project, project_dir)

    # def remove_project_file(self, filename):
    #     super().remove_project_file(filename)

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Utility functions used to implement the API:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    @staticmethod
    def hsdev_version(exec_with, install_dir):
        retval = [0, 0, 0, 0]
        hsdev_proc = ProcHelper.exec_with_wrapper(exec_with, install_dir, ['hsdev', 'version'])
        if hsdev_proc.process is not None:
            exit_code, out, _ = hsdev_proc.wait()
            if exit_code == 0:
                ## 'cabal new-run' can spit out multiple lines of status before executing the task:
                for line in out.splitlines():
                    hsver = re.match(r'(?P<major>\d+)\.(?P<minor>\d+)\.(?P<revision>\d+)\.(?P<build>\d+)', line)
                    if hsver:
                        major = int(hsver.group('major'))
                        minor = int(hsver.group('minor'))
                        revision = int(hsver.group('revision'))
                        build = int(hsver.group('build'))
                        retval = [major, minor, revision, build]
                        break

        return retval


    @staticmethod
    def concat_args(args):
        def inner_concat(left, right):
            (left_pred, left_expr) = left
            (right_pred, right_expr) = right
            return (left_pred or right_pred, (left_expr if left_pred else []) + (right_expr if right_pred else []))

        return reduce(inner_concat, args, (True, []))[1]

    def files_and_contents(self, files, contents):
        contents = contents or {}
        retval = [{'file': f, 'contents': contents.get(f)} for f in files] if files else []
        return  retval

    def make_callbacks(self, name, on_response=None, result_convert=result_identity, on_notify=None, on_error=None,
                       **backend_args):
        with self.serial_lock:
            req_serial = str(self.request_serial)
            self.request_serial += 1

        # Clean up backend arguments:
        for param in ['on_response', 'result_convert', 'on_notify', 'on_error']:
            if param in backend_args:
                del backend_args[param]

        return (HsCallback.HsDevCallbacks(req_serial, name, on_response, result_convert, on_notify, on_error), backend_args)

    def hsdev_command(self, name, opts, callbacks, async_cmd=False, timeout=HSDEV_CALL_TIMEOUT, is_list=False,
                      on_result_part=None, split_result=None):
        if split_result is None:
            split_res = on_result_part is not None

        if is_list and split_res:
            result = []

            def hsdev_command_notify(reply):
                if 'result-part' in reply:
                    notify_result = callbacks.call_result_convert([reply['result-part']])[0]
                    on_result_part(notify_result)
                    result.append(notify_result)
                else:
                    callbacks.call_notify(reply)

            # FIXME: Is this option still used?
            opts.update({'split-result': None})
            callbacks.add_notify(hsdev_command_notify)

        resp = self.client.call(name, opts, callbacks, wait=not async_cmd, timeout=timeout)
        return resp


    def command(self, name, opts, callbacks, timeout=HSDEV_CALL_TIMEOUT, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, callbacks, async_cmd=False, timeout=timeout, is_list=False,
                                  on_result_part=on_result_part, split_result=split_result)


    def async_command(self, name, opts, callbacks, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, callbacks, async_cmd=True, timeout=None, is_list=False,
                                  on_result_part=on_result_part, split_result=split_result)


    def list_command(self, name, opts, callbacks, timeout=HSDEV_CALL_TIMEOUT, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, callbacks, async_cmd=False, timeout=timeout, is_list=True,
                                  on_result_part=on_result_part, split_result=split_result)

    def async_list_command(self, name, opts, callbacks, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, callbacks, async_cmd=True, timeout=None, is_list=True,
                                  on_result_part=on_result_part, split_result=split_result)

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # API implementation:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def link(self, hold=False):
        return self.command('link', {'hold': hold}, self.make_callbacks('link')[0])

    def ping(self):
        return self.command('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong'),
                            self.make_callbacks('ping')[0])

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False, wait_complete=False, **backend_args):
        action = self.command if wait_complete else self.async_command
        callbacks, backend_args = self.make_callbacks('scan', **backend_args)

        return action('scan', {'projects': projects or [],
                               'cabal': cabal,
                               'sandboxes': sandboxes or [],
                               'files': self.files_and_contents(files, contents),
                               'paths': paths or [],
                               'ghc-opts': ghc or [],
                               'docs': docs,
                               'infer': infer},
                      callbacks, **backend_args)

    def docs(self, projects=None, files=None, modules=None, **backend_args):
        callbacks, backend_args = self.make_callbacks('docs', **backend_args)
        return self.async_command('docs', {'projects': projects or [],
                                           'files': files or [],
                                           'modules': modules or []},
                                  callbacks, **backend_args)

    def infer(self, projects=None, files=None, modules=None, **backend_args):
        callbacks, backend_args = self.make_callbacks('infer', **backend_args)
        return self.async_command('infer', {'projects': projects or [],
                                            'files': files or [],
                                            'modules': modules or []},
                                  callbacks, **backend_args)

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None, **backend_args):
        callbacks, backend_args = self.make_callbacks('remove', **backend_args)
        return self.async_list_command('remove', {'projects': projects or [],
                                                  'cabal': cabal,
                                                  'sandboxes': sandboxes or [],
                                                  'files': files or [],
                                                  'packages': packages or []},
                                       callbacks, **backend_args)

    def remove_all(self, **backend_args):
        callbacks, backend_args = self.make_callbacks('remove-all', **backend_args)
        return self.command('remove-all', {}, callbacks, **backend_args)

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

        callbacks, backend_args = self.make_callbacks('modules', result_convert=ResultParse.parse_modules_brief, **backend_args)
        return self.list_command('modules', {'filters': filters}, callbacks, **backend_args)

    def list_packages(self, **backend_args):
        callbacks, backend_args = self.make_callbacks('packages', **backend_args)
        return self.list_command('packages', {}, callbacks, **backend_args)

    def list_projects(self, **backend_args):
        callbacks, backend_args = self.make_callbacks('projects', **backend_args)
        return self.list_command('projects', {}, callbacks, **backend_args)

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

        callbacks, backend_args = self.make_callbacks('symbol', result_convert=ResultParse.parse_decls, **backend_args)
        return self.list_command('symbol', {'query': query, 'filters': filters, 'locals': local_names},
                                 callbacks, **backend_args)

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

        callbacks, backend_args = self.make_callbacks('module', result_convert=ResultParse.parse_modules, **backend_args)
        return self.command('module', {'query': query, 'filters': filters}, callbacks, **backend_args)

    def resolve(self, file, exports=False, **backend_args):
        callbacks, backend_args = self.make_callbacks('resolve', result_convert=ResultParse.parse_module, **backend_args)
        return self.command('resolve', {'file': file, 'exports': exports}, callbacks, **backend_args)

    def project(self, project=None, path=None, **backend_args):
        callbacks, backend_args = self.make_callbacks('project', **backend_args)
        return self.command('project', {'name': project} if project else {'path': path}, callbacks, **backend_args)

    def sandbox(self, path, **backend_args):
        callbacks, backend_args = self.make_callbacks('sandbox', **backend_args)
        return self.command('sandbox', {'path': path}, callbacks, **backend_args)

    def lookup(self, name, file, **backend_args):
        callbacks, backend_args = self.make_callbacks('lookup', result_convert=ResultParse.parse_decls, **backend_args)
        return self.list_command('lookup', {'name': name, 'file': file}, callbacks, **backend_args)

    def whois(self, name, file, **backend_args):
        callbacks, backend_args = self.make_callbacks('whois', result_convert=ResultParse.parse_declarations, **backend_args)
        return self.list_command('whois', {'name': name, 'file': file}, callbacks, **backend_args)

    def scope_modules(self, _projcname, file, lookup='', search_type='prefix', **backend_args):
        callbacks, backend_args = self.make_callbacks('scope_modules', result_convert=ResultParse.parse_modules_brief,
                                                      **backend_args)
        return self.list_command('scope modules', {'query': {'input': lookup, 'type': search_type}, 'file': file},
                                 callbacks, **backend_args)

    def scope(self, file, lookup='', search_type='prefix', global_scope=False, **backend_args):
        callbacks, backend_args = self.make_callbacks('scope', result_convert=ResultParse.parse_declarations, **backend_args)
        return self.list_command('scope',
                                 {'query': {'input': lookup,
                                            'type': search_type
                                           },
                                  'global': global_scope,
                                  'file': file
                                 }, callbacks, **backend_args)

    def complete(self, sym, file, wide=False, **backend_args):
        qname = sym.qualified_name() if sym.name is not None else sym.module + '.'
        if 'contents' in backend_args:
            ## contents = backend_args['contents']
            del backend_args['contents']
        callbacks, backend_args = self.make_callbacks('complete', result_convert=ResultParse.parse_declarations, **backend_args)
        return self.list_command('complete',
                                 {'prefix': qname,
                                  'wide': wide,
                                  'file': file},
                                 callbacks, **backend_args)

    def hayoo(self, query, page=None, pages=None, **backend_args):
        callbacks, backend_args = self.make_callbacks('hayoo', result_convert=ResultParse.parse_decls, **backend_args)
        return self.list_command('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1}, callbacks, **backend_args)

    def cabal_list(self, packages, **backend_args):
        def convert_to_cabal_packages(pkg_list):
            return [ResultParse.parse_cabal_package(pkg) for pkg in pkg_list] if pkg_list else None

        callbacks, backend_args = self.make_callbacks('cabal list', result_convert=convert_to_cabal_packages, **backend_args)
        return self.list_command('cabal list', {'packages': packages}, callbacks, **backend_args)

    def lint(self, files=None, contents=None, hlint=None, wait_complete=False, **backend_args):
        action = self.list_command if wait_complete else self.async_list_command
        result_convert = backend_args.pop('result_convert', [])
        if result_convert and not isinstance(result_convert, list):
            result_convert = [result_convert]
        result_convert.append(self.convert_warnings)

        callbacks, backend_args = self.make_callbacks('lint', result_convert=result_convert, **backend_args)
        return action('lint', {'files': self.files_and_contents(files, contents),
                               'hlint-opts': hlint or []},
                      callbacks, **backend_args)

    def check(self, files=None, contents=None, ghc=None, wait_complete=False, **backend_args):
        action = self.list_command if wait_complete else self.async_list_command
        callbacks, backend_args = self.make_callbacks('check', **backend_args)
        return action('check', {'files': self.files_and_contents(files, contents),
                                'ghc-opts': ghc or []},
                      callbacks, **backend_args)

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None, wait_complete=False, **backend_args):
        action = self.list_command if wait_complete else self.async_list_command
        result_convert = backend_args.pop('result_convert', [])
        if result_convert and not isinstance(result_convert, list):
            result_convert = [result_convert]
        result_convert.append(self.convert_warnings)

        callbacks, backend_args = self.make_callbacks('check-lint', result_convert=result_convert, **backend_args)
        return action('check-lint', {'files': self.files_and_contents(files, contents),
                                     'ghc-opts': ghc or [],
                                     'hlint-opts': hlint or []},
                      callbacks, **backend_args)

    def types(self, _projectname, file, _modulename, _line, _column, ghc_flags=None, contents=None, **backend_args):
        callbacks, backend_args = self.make_callbacks('types', **backend_args)
        return self.list_command('types', {'files': self.files_and_contents(file, contents),
                                           'ghc-opts': ghc_flags or []},
                                 callbacks, **backend_args)

    def langs(self, _projectname, **backend_args):
        callbacks, backend_args = self.make_callbacks('langs', **backend_args)
        return self.command('langs', {}, callbacks, **backend_args)

    def flags(self, _projectname, **backend_args):
        callbacks, backend_args = self.make_callbacks('flags', **backend_args)
        return self.command('flags', {}, callbacks, **backend_args)

    def autofix_show(self, messages, **backend_args):
        wait_complete = backend_args.get('wait_complete', True)
        backend_args.pop('wait_complete', None)
        action = self.list_command if wait_complete else self.async_list_command
        callbacks, backend_args = self.make_callbacks('autofix show', result_convert=ResultParse.parse_corrections,
                                                      **backend_args)
        return action('autofix show', {'messages': messages}, callbacks, **backend_args)

    def autofix_fix(self, messages, rest=None, pure=False, **backend_args):
        callbacks, backend_args = self.make_callbacks('autofix fix', result_convert=ResultParse.parse_corrections,
                                                      **backend_args)
        return self.list_command('autofix fix', {'messages': messages, 'rest': rest or [], 'pure': pure},
                                 callbacks, **backend_args)

    def ghc_eval(self, exprs, file=None, source=None, **backend_args):
        the_file = None
        if file is not None:
            the_file = {'file': the_file, 'contents': source}
        callbacks, backend_args = self.make_callbacks('ghc eval', **backend_args)
        return self.list_command('ghc eval', {'exprs': exprs, 'file': the_file}, callbacks, **backend_args)

    def exit(self):
        return self.command('exit', {}, self.make_callbacks('exit')[0])

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Advanced features:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def query_import(self, symname, filename):
        if self.whois(symname, filename):
            return (False, ['Symbol {0} already in scope'.format(symname)])

        candidates = self.lookup(symname, filename)
        return (True, candidates) if candidates else (False, ['Symbol {0} not found'.format(symname)])

    def contents_to_module(self, contents):
        imp_module = None
        hsinspect_proc = ProcHelper.exec_with_wrapper(self.exec_with, self.install_dir, ['hsinspect'])
        if hsinspect_proc.process is not None:
            exit_code, result, _ = hsinspect_proc.wait(input_str=contents)
            if exit_code == 0:
                pyresult = json.loads(result).get('result')
                if pyresult is not None:
                    if Logging.is_log_level(Logging.LOG_DEBUG):
                        pprint.pprint(pyresult, width=127)
                    modinfo = pyresult.get('module')
                    if modinfo is not None:
                        imp_module = ResultParse.parse_module(modinfo)

        return imp_module

    def clean_imports(self, filename):
        cmd = ['hsclearimports', filename, '--max-import-list', '64']
        hsclean_proc = ProcHelper.exec_with_wrapper(self.exec_with, self.install_dir, cmd)
        if hsclean_proc.process is not None:
            exit_code, result, err = hsclean_proc.wait()
            if exit_code == 0:
                return (True, result.splitlines())

            return (False, err)

        return (False, ['\'hscleanimports\' utility not found.'])

    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
    # Utility functions:
    # -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-

    def convert_warnings(self, messages):
        for msg in messages:
            if msg.get('level', '') == 'warning':
                msg['level'] = 'hint'

        return messages


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
