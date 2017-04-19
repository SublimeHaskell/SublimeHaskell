"""
"""

import json
import socket
import sys
import threading
import time
import traceback

import SublimeHaskell.internals.locked_object as LockedObject
import SublimeHaskell.hsdev.callback as HsCallback
import SublimeHaskell.hsdev.result_parse as ResultParse
import SublimeHaskell.internals.logging as Logging


def cmd(name_, opts_, on_result=lambda r: r):
    return (name_, opts_, on_result)


def files_and_contents(files, contents):
    return [{'file': f, 'contents': None} for f in files] + [{'file': f, 'contents': cts} for f, cts in contents.items()]

# hsdev client
# see for functions with command decorator for hsdev api
class HsDevClient(object):
    # Number of times we try to connect to the hsdev server
    CONNECT_TRIES = 10
    # Delay between attempts if not successful
    CONNECT_DELAY = 2.0

    def __init__(self, host, port):
        self.host = host
        self.port = port
        self.socket = None
        self.listener = None
        self.request_map = LockedObject.LockedObject({})
        self.request_serial = 1

        self.part = ''

        self.on_connected = None
        self.on_disconnected = None
        self.on_reconnect = None

    def __del__(self):
        self.close()

    # Socket functions

    def connect(self):
        try:
            for retry in range(1, HsDevClient.CONNECT_TRIES):
                Logging.log('connecting to hsdev server (attempt {0})...'.format(retry), Logging.LOG_INFO)

                # Use 'localhost' instead of the IP dot-quad for systems (and they exist) that are solely
                # IPv6. Makes this code friendlier to IPv4 and IPv6 hybrid systems.
                self.socket = socket.create_connection((self.host, self.port))
                self.listener = threading.Thread(target=self.listen)
                self.listener.start()

                Logging.log('Connection established to \'hsdev\' server.', Logging.LOG_INFO)
                HsCallback.call_callback(self.on_connected, name='HsDev.on_connected')
                return True

        except OSError:
            # Captures all of the socket exceptions:
            Logging.log('Failed to connect to hsdev server:', Logging.LOG_WARNING)
            print(traceback.format_exc())
            time.sleep(HsDevClient.CONNECT_DELAY)

        return False

    def close(self):
        self.socket.close()
        self.socket = None

    def is_connected(self):
        return self.socket is not None

    def on_receive(self, ident, command, on_response=None, on_notify=None, on_error=None):
        with self.request_map as requests:
            requests[ident] = HsCallback.HsDevCallbacks(ident, command, on_response, on_notify, on_error)

    def verify_connected(self):
        return self.is_connected()

    def connection_lost(self, func_name, reason):
        self.close()
        Logging.log('{0}: connection to hsdev lost: {1}'.format(func_name, reason), Logging.LOG_ERROR)
        HsCallback.call_callback(self.on_disconnected, name='HsDev.on_disconnected')

        # send error to callbacks
        with self.request_map as requests:
            for on_msg in requests.values():
                on_msg.on_error('connection lost', {})
            requests.clear()

        self.request_serial = 1
        self.part = ''

    def call(self, command, opts=None, on_response=None, on_notify=None, on_error=None, wait=False, timeout=None):
        # log
        args_cmd = 'hsdev {0}'.format(command)
        call_cmd = 'hsdev {0} with {1}'.format(command, opts)

        if not self.verify_connected():
            return None if wait else False

        try:
            opts = opts or {}
            wait_receive = threading.Event() if wait else None
            result_dict = {}

            def on_response_(resp):
                result_dict['result'] = resp
                HsCallback.call_callback(on_response, resp)
                if wait_receive:
                    wait_receive.set()

            def on_error_(exc, details):
                HsCallback.call_callback(on_error, exc, details)
                if wait_receive:
                    wait_receive.set()

            req_serial = str(self.request_serial)
            self.request_serial = self.request_serial + 1

            if wait or on_response or on_notify or on_error:
                self.on_receive(req_serial, args_cmd, on_response_, on_notify, on_error_)

            opts.update({'no-file': True})
            opts.update({'id': req_serial, 'command': command})
            msg = json.dumps(opts, separators=(',', ':'))

            # The first sendall sends the request -- if the socket encounters an error, it isn't reported
            # immediately. It is actually reported on the next send() or sendall() [nutshell: TCP acks the
            # first sendall(), but if the remote side closes the socket, you have to wait until the next
            # TCP ack to know that that's been done.] OTOH, the first sendall() can also fail...
            self.socket.sendall(msg.encode('utf-8'))
            self.socket.sendall('\n'.encode('utf-8'))
            Logging.log(call_cmd, Logging.LOG_TRACE)

            if wait:
                wait_receive.wait(timeout)
                return result_dict.get('result')
            else:
                return True

        except OSError:
            Logging.log('{0} fails with exception, see traceback in console window.'.format(call_cmd), Logging.LOG_ERROR)
            print(traceback.format_exc())
            self.connection_lost('call', sys.exc_info()[1])
            return False


    def hsdev_command(self, name, opts, on_result, async=False, timeout=None, is_list=False, wait_flag=None, on_response=None,
                      on_notify=None, on_error=None, on_result_part=None, split_result=None):
        if wait_flag is None:
            wait_flag = not async
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

            opts.update({'split-result': None})  # FIXME: Is this option still used?
            resp = self.call(name
                             , opts
                             , on_response=on_response
                             , on_notify=inner_notify
                             , on_error=on_error
                             , wait=wait_flag
                             , timeout=timeout)

            return result if wait_flag else resp

        else:
            def processed_response(resp):
                on_response(on_result(resp))

            resp = self.call(name
                             , opts
                             , on_response=processed_response if on_response else None
                             , on_notify=on_notify
                             , on_error=on_error
                             , wait=wait_flag
                             , timeout=timeout)

            return on_result(resp) if wait_flag else resp

    def command(self, name, opts, on_result, is_list=False, wait_flag=None, on_response=None, on_notify=None, on_error=None,
                on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=False, timeout=1, is_list=is_list, wait_flag=wait_flag,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)


    def async_command(self, name, opts, on_result, is_list=False, wait_flag=None, on_response=None, on_notify=None,
                      on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=True, timeout=None, is_list=is_list, wait_flag=wait_flag,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)


    def list_command(self, name, opts, on_result, is_list=False, wait_flag=None, on_response=None, on_notify=None,
                     on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=False, timeout=1, is_list=is_list, wait_flag=wait_flag,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)

    def async_list_command(self, name, opts, on_result, is_list=True, wait_flag=None, on_response=None, on_notify=None,
                           on_error=None, on_result_part=None, split_result=None):
        return self.hsdev_command(name, opts, on_result, async=True, timeout=None, is_list=is_list, wait_flag=wait_flag,
                                  on_response=on_response, on_notify=on_notify, on_error=on_error,
                                  on_result_part=on_result_part, split_result=split_result)


    def listen(self):
        while self.verify_connected():
            try:
                resp = json.loads(self.get_response())
                if 'id' in resp:
                    callbacks = None
                    with self.request_map as requests:
                        if resp['id'] in requests:
                            callbacks = requests[resp['id']]
                    if callbacks:
                        if 'notify' in resp:
                            callbacks.call_notify(resp['notify'])
                        if 'error' in resp:
                            err = resp.pop("error")
                            callbacks.call_error(err, resp)
                            with self.request_map as requests:
                                requests.pop(resp['id'])
                        if 'result' in resp:
                            callbacks.call_response(resp['result'])
                            with self.request_map as requests:
                                requests.pop(resp['id'])
            except IOError:
                self.connection_lost('listen', sys.exc_info()[1])
                return

    def get_response(self):
        while '\n' not in self.part:
            self.part = self.part + self.socket.recv(65536).decode('utf-8')
        (pre, _, post) = self.part.partition('\n')
        self.part = post
        return pre

    # Commands

    def link(self, hold=False):
        resp = self.command(*cmd('link', {'hold': hold}))
        Logging.log('HsDevClient.link: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def ping(self):
        resp = self.command(*cmd('ping', {}, lambda r: r and ('message' in r) and (r['message'] == 'pong')))
        Logging.log('HsDevClient.ping: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def scan(self, cabal=False, sandboxes=None, projects=None, files=None, paths=None, ghc=None, contents=None,
             docs=False, infer=False):
        resp = self.async_command(*cmd('scan', {'projects': projects or [],
                                                'cabal': cabal,
                                                'sandboxes': sandboxes or [],
                                                'files': files_and_contents(files or [], contents or {}),
                                                'paths': paths or [],
                                                'ghc-opts': ghc or [],
                                                'docs': docs,
                                                'infer': infer}))
        Logging.log('HsDevClient.scan: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def docs(self, projects=None, files=None, modules=None):
        resp = self.async_command(*cmd('docs', {'projects': projects or [],
                                                'files': files or [],
                                                'modules': modules or []}))
        Logging.log('HsDevClient.docs: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def infer(self, projects=None, files=None, modules=None):
        resp = self.async_command(*cmd('infer', {'projects': projects or [],
                                                 'files': files or [],
                                                 'modules': modules or []}))
        Logging.log('HsDevClient.infer: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def remove(self, cabal=False, sandboxes=None, projects=None, files=None, packages=None):
        resp = self.async_list_command(*cmd('remove', {'projects': projects or [],
                                                       'cabal': cabal,
                                                       'sandboxes': sandboxes or [],
                                                       'files': files or [],
                                                       'packages': packages or []}))
        Logging.log('HsDevClient.remove: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def remove_all(self):
        resp = self.command(*cmd('remove-all', {}))
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

        resp = self.list_command(*cmd('modules', {'filters': filters}, ResultParse.parse_modules_brief))
        Logging.log('HsDevClient.list_modules: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def list_packages(self):
        resp = self.list_command(*cmd('packages', {}))
        Logging.log('HsDevClient.list_packages: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def list_projects(self):
        resp = self.list_command(*cmd('projects', {}))
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

        resp = self.list_command(*cmd('symbol', {'query': query, 'filters': filters, 'locals': local_names},
                                      ResultParse.parse_decls))
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

        resp = self.command(*cmd('module', {'query': query, 'filters': filters}, ResultParse.parse_modules))
        Logging.log('HsDevClient.module: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def resolve(self, file, exports=False):
        resp = self.command(*cmd('resolve', {'file': file, 'exports': exports}, ResultParse.parse_module))
        Logging.log('HsDevClient.resolve: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def project(self, project=None, path=None):
        resp = self.command(*cmd('project', {'name': project} if project else {'path': path}))
        Logging.log('HsDevClient.project: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def sandbox(self, path):
        resp = self.command(*cmd('sandbox', {'path': path}))
        Logging.log('HsDevClient.sandbox: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def lookup(self, name, file):
        resp = self.list_command(*cmd('lookup', {'name': name, 'file': file}, ResultParse.parse_decls))
        Logging.log('HsDevClient.lookup: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def whois(self, name, file):
        resp = self.list_command(*cmd('whois', {'name': name, 'file': file}, ResultParse.parse_declarations))
        Logging.log('HsDevClient.whois: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def scope_modules(self, file, lookup='', search_type='prefix'):
        resp = self.list_command(*cmd('scope modules', {'query': {'input': lookup, 'type': search_type}, 'file': file},
                                      ResultParse.parse_modules_brief))
        Logging.log('HsDevClient.modules: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def scope(self, file, lookup='', search_type='prefix', global_scope=False):
        resp = self.list_command(*cmd('scope',
                                      {'query': {'input': lookup,
                                                 'type': search_type
                                                },
                                       'global': global_scope,
                                       'file': file
                                      }, ResultParse.parse_declarations))
        Logging.log('HsDevClient.scope: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def complete(self, lookup, file, wide=False):
        resp = self.list_command(*cmd('complete', {'prefix': lookup, 'wide': wide, 'file': file},
                                      ResultParse.parse_declarations))
        Logging.log('HsDevClient.complete: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def hayoo(self, query, page=None, pages=None):
        resp = self.list_command(*cmd('hayoo', {'query': query, 'page': page or 0, 'pages': pages or 1},
                                      ResultParse.parse_decls))
        Logging.log('HsDevClient.hayoo: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def cabal_list(self, packages):
        resp = self.list_command(*cmd('cabal list', {'packages': packages},
                                      lambda r: [ResultParse.parse_cabal_package(s) for s in r] if r else None))
        Logging.log('HsDevClient.cabal_list: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def lint(self, files=None, contents=None, hlint=None):
        resp = self.list_command(*cmd('lint', {'files': files_and_contents(files or [], contents or {}),
                                               'hlint-opts': hlint or []}))
        Logging.log('HsDevClient.lint: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def check(self, files=None, contents=None, ghc=None):
        resp = self.list_command(*cmd('check', {'files': files_and_contents(files or [], contents or {}),
                                                'ghc-opts': ghc or []}))
        Logging.log('HsDevClient.check: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def check_lint(self, files=None, contents=None, ghc=None, hlint=None):
        resp = self.list_command(*cmd('check-lint', {'files': files_and_contents(files or [], contents or {}),
                                                     'ghc-opts': ghc or [],
                                                     'hlint-opts': hlint or []}))
        Logging.log('HsDevClient.check_lint: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def types(self, files=None, contents=None, ghc=None):
        resp = self.list_command(*cmd('types', {'files': files_and_contents(files or [], contents or {}),
                                                'ghc-opts': ghc or []}))
        Logging.log('HsDevClient.types: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def langs(self):
        resp = self.command(*cmd('langs', {}))
        Logging.log('HsDevClient.langs: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def flags(self):
        resp = self.command(*cmd('flags', {}))
        Logging.log('HsDevClient.flags: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def autofix_show(self, messages):
        resp = self.list_command(*cmd('autofix show', {'messages': messages}, ResultParse.parse_corrections))
        Logging.log('HsDevClient.autofix_show: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def autofix_fix(self, messages, rest=None, pure=False):
        resp = self.autofix_fix(*cmd('autofix fix', {'messages': messages, 'rest': rest or [], 'pure': pure},
                                     ResultParse.parse_corrections))
        Logging.log('HsDevClient.autofix_fix: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def ghc_eval(self, exprs, file=None, source=None):
        the_file = None
        if file is not None:
            the_file = {'file': the_file, 'contents': source}
        resp = self.list_command(*cmd('ghc eval', {'exprs': exprs, 'file': the_file}))
        Logging.log('HsDevClient.ghc_eval: {0}'.format(resp), Logging.LOG_DEBUG)
        return resp

    def exit(self):
        return self.command(*cmd('exit', {}))
