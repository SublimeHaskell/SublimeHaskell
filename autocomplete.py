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

# If true, files that have not changed will not be re-inspected.
CHECK_MTIME = True

MODULE_INSPECTOR_SOURCE_PATH = os.path.join(PACKAGE_PATH, 'ModuleInspector.hs')
MODULE_INSPECTOR_EXE_PATH = os.path.join(PACKAGE_PATH, 'ModuleInspector')
MODULE_INSPECTOR_OBJ_DIR = os.path.join(PACKAGE_PATH, 'obj')

OUTPUT_PATH = os.path.join(PACKAGE_PATH, 'module_info.cache')

class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    def __init__(self):
        # TODO: Start the InspectorAgent as a separate thread.
        self.inspector = InspectorAgent()
        self.inspector.run()
        self.info_lock = threading.Lock()
        self.info = {}

    def on_query_completions(self, view, prefix, locations):
        return self.get_completion_list(view.file_name())

    def on_post_save(self, view):
        # TODO: Update the module info once per second, at most.
        # TODO: Make sure the entire project's module info is up-to-date.
        cabal_dir = get_cabal_project_dir_of_view(view)
        if cabal_dir is not None:
            log('reinspecting project ({0})'.format(cabal_dir))
            self.refresh_all_module_info(cabal_dir)
        else:
            log('no inspection; file is not in a Cabal project')

    def refresh_all_module_info(self, base_dir):
        "Rebuild module information for all files under the specified directory."
        begin_time = time.clock()
        files_in_dir = list_files_in_dir_recursively(base_dir)
        haskell_source_files = [x for x in files_in_dir if x.endswith('.hs')]
        for filename in haskell_source_files:
            self.refresh_module_info(filename)
        end_time = time.clock()
        log('total inspection time: {0} seconds'.format(end_time - begin_time))

    def refresh_module_info(self, filename):
        "Rebuild module information for the specified file."
        # TODO: Only do this within Haskell files in Cabal projects.
        # TODO: Skip this file if it hasn't changed since it was last inspected.
        # If the file hasn't changed since it was last inspected, do nothing:
        modificationTime = os.stat(filename).st_mtime
        if CHECK_MTIME:
            inspectionTime = self.get_inspection_time_of_file(filename)
            if modificationTime <= inspectionTime:
                log('skipping inspection; module info is up to date ({0})'.format(
                    filename))
                return
        log('inspecting module ({0})'.format(filename))
        exit_code, stdout, stderr = call_and_wait(
            [MODULE_INSPECTOR_EXE_PATH, filename])
        new_info = json.loads(stdout)
        # Remember when this info was collected.
        new_info['inspectedAt'] = modificationTime
        # Dump the currently-known module info to disk:
        formatted_json = json.dumps(self.info, indent=2)
        with open(OUTPUT_PATH, 'w') as f:
            f.write(formatted_json)
        with self.info_lock:
            self.info[filename] = new_info

    def get_inspection_time_of_file(self, filename):
        """Return the time that a file was last inspected.
        Return zero if it has never been inspected."""
        with self.info_lock:
            try:
                return self.info[filename]['inspectedAt']
            except KeyError:
                return 0.0

    def get_completion_list(self, current_file_name):
        "Get all the completions that apply to the current file."
        # TODO: Filter according to what names the current file has in scope.
        completions = []
        with self.info_lock:
            for file_name, file_info in self.info.items():
                if 'error' in file_info:
                    # There was an error parsing this file; skip it.
                    continue
                for d in file_info['declarations']:
                    identifier = d['identifier']
                    declaration_info = d['info']
                    # TODO: Show the declaration info somewhere.
                    completions.append(
                        (identifier[:MAX_COMPLETION_LENGTH], identifier))
        return completions

class InspectorAgent(threading.Thread):
    def __init__(self):
        self.dirty_files_lock = threading.Lock()
        self.dirty_files = []

    def run(self):
        # Compile the ModuleInspector:
        exit_code, out, err = call_and_wait(['ghc',
            '--make', MODULE_INSPECTOR_SOURCE_PATH,
            '-o', MODULE_INSPECTOR_EXE_PATH,
            '-outputdir', MODULE_INSPECTOR_OBJ_DIR])
        # TODO: If compilation failed, we can't proceed; handle this.
        # TODO: Once per second, reinspect any projects marked dirty.

    def mark_file_dirty(self, filename):
        with self.dirty_files_lock:
            self.dirty_files.append(filename)

def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files
