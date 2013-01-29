import re
import os
import sublime_plugin

from sublime_haskell_common import *

def ghci_info(module, name):
    """
    Returns info for name as dictionary with fields:
    'module' -> module
    'name' -> name
    'what' -> one of 'function', 'class' or 'data'
    'type' -> type of function (e.g. 'Monad m => (a -> m b) -> [a] -> m [b]' for mapM)
    'args' -> args of 'class' or 'data', list of names (e.g. ['k', 'a'] for Data.Map.Map)
    'ctx' -> context of 'class' or 'data'
    """
    ghci_cmd = [
        ":m + " + module,
        ":i " + module + "." + name,
        ":q"]
    (exit_code, stdout, stderr) = call_and_wait_with_input(ghci_append_package_db(['ghci']), "\n".join(ghci_cmd))
    stdout = crlf2lf(stdout)
    if exit_code == 0:
        functionRegex = '{0}\s+::\s+(?P<type>.*?)(\s+--(.*))?$'.format(name)
        dataRegex = '(?P<what>data)\s+((?P<ctx>(.*))=>\s+)?{0}\s+(?P<args>(\w+\s+)*)='.format(name)
        classRegex = '(?P<what>class)\s+((?P<ctx>(.*))=>\s+)?{0}\s+(?P<args>(\w+\s+)*)(.*)where$'.format(name)

        result = {
            'module': module,
            'name': name }

        if name[0].isupper():
            # data or class
            matched = re.search(dataRegex, stdout, re.MULTILINE) or re.search(classRegex, stdout, re.MULTILINE)
            if matched:
                result['what'] = matched.group('what')
                result['args'] = matched.group('args').strip().split(' ') if matched.group('args') else []
                if matched.group('ctx'):
                    result['ctx'] = matched.group('ctx')
                return result
        else:
            # function
            matched = re.search(functionRegex, stdout, re.MULTILINE)
            if matched:
                result['what'] = 'function'
                result['type'] = matched.group('type')
                return result
    return None

def ghci_package_db():
    dev = get_setting_async('use_cabal_dev')
    box = get_setting_async('cabal_dev_sandbox')
    if dev and box:
        package_conf = (filter(lambda x: re.match('packages-(.*)\.conf', x), os.listdir(box)) + [None])[0]
        if package_conf:
            return os.path.join(box, package_conf)
    return None

def ghci_append_package_db(cmd):
    package_conf = ghci_package_db()
    if package_conf:
        cmd.extend(['-package-db', package_conf])
    return cmd

# Opens REPL in SublimeREPL
class SublimeHaskellReplOpenCommand(sublime_plugin.WindowCommand):
    def run(self):
        self.window.run_command("repl_open", {
            "type": "subprocess",
            "encoding": "utf8",
            "cmd": ghci_append_package_db(["ghci"]),
            "cwd": "$file_path",
            "external_id": "sublime_haskell",
            "syntax": "Packages/Haskell/Haskell.tmLanguage" })
