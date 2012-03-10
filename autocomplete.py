from common import *
import json
import os
import sublime
import sublime_plugin
import subprocess
import threading
import time

# Completion text longer than this is ellipsized:
MAX_COMPLETION_LENGTH = 37

class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    def __init__(self):
        # TODO: Recompile the ModuleInspector:
        # call_and_wait(['ghc', '--make', 'ModuleInspector.hs', '-outputdir', 'obj'])
        self.info_lock = threading.Lock()
        self.info = {}

    def on_query_completions(self, view, prefix, locations):
        return self.get_completion_list(view.file_name())

    def on_post_save(self, view):
        # TODO: Update the module info once per second, at most.
        # TODO: Make sure the entire project's module info is up-to-date.
        self.refresh_module_info(view.file_name())

    def refresh_all_module_info(self, base_dir):
        "Rebuild module information for all files under the specified directory."
        files_in_dir = list_files_in_dir_recursively(base_dir)
        haskell_source_files = [x for x in files_in_dir if x.endswith('.hs')]
        for filename in haskell_source_files:
            self.refresh_module_info(filename)

    def refresh_module_info(self, filename):
        "Rebuild module information for the specified file."
        # TODO: Only do this within Haskell files in Cabal projects.
        # TODO: Skip this file if it hasn't changed since it was last inspected.
        print('Inspecting "{0}"...'.format(filename))
        exit_code, stdout, stderr = call_and_wait(
            ['runhaskell', 'ModuleInspector.hs', filename])
        new_info = json.loads(stdout)
        # Remember when this info was collected.
        new_info['collectedAt'] = time.time()
        # Dump the currently-known module info to disk:
        formatted_json = json.dumps(self.info, indent=2)
        with open('module_info.cache', 'w') as f:
            f.write(formatted_json)
        with self.info_lock:
            self.info[filename] = new_info

    def get_completion_list(self, current_file_name):
        "Get all the completions that apply to the current file."
        # TODO: Filter according to what names the current file has in scope.
        completions = []
        with self.info_lock:
            for file_name, file_info in self.info.items():
                if 'error' in file_info:
                    # There was an error parsing this file; skip it.
                    log('skip!')
                    continue
                for d in file_info['declarations']:
                    identifier = d['identifier']
                    declaration_info = d['info']
                    # TODO: Show the declaration info somewhere.
                    completions.append(
                        (identifier[:MAX_COMPLETION_LENGTH], identifier))
        return completions

def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files
