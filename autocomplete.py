import json
import os
import re
import sublime
import sublime_plugin
import subprocess
import threading
import time

from sublime_haskell_common import PACKAGE_PATH, get_cabal_project_dir_of_file, get_cabal_project_dir_of_view, call_and_wait, call_ghcmod_and_wait, log

# Completion text longer than this is ellipsized:
MAX_COMPLETION_LENGTH = 37

# If true, files that have not changed will not be re-inspected.
CHECK_MTIME = True

MODULE_INSPECTOR_SOURCE_PATH = os.path.join(PACKAGE_PATH, 'ModuleInspector.hs')
MODULE_INSPECTOR_EXE_PATH = os.path.join(PACKAGE_PATH, 'ModuleInspector')
MODULE_INSPECTOR_OBJ_DIR = os.path.join(PACKAGE_PATH, 'obj')

OUTPUT_PATH = os.path.join(PACKAGE_PATH, 'module_info.cache')

# The agent sleeps this long between inspections.
AGENT_SLEEP_DURATION = 5.0

# Checks if we are in a LANGUAGE pragma.
LANGUAGE_RE = re.compile(r'.*{-#\s+LANGUAGE.*')

# Checks if we are in an import statement.
IMPORT_RE = re.compile(r'.*import(\s+qualified)?\s+')
IMPORT_QUALIFIED_POSSIBLE_RE = re.compile(r'.*import\s+(?P<qualifiedprefix>\S*)$')


def get_line_contents(view, location):
    """
    Returns the contents of the line at the given location.
    """
    return view.substr(sublime.Region(view.line(location).a, location))


class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    def __init__(self):
        # TODO: Start the InspectorAgent as a separate thread.
        self.inspector = InspectorAgent()
        self.inspector.start()

        self.language_completions = []

        self.init_ghcmod_completions()

    # Gets available LANGUAGE options and import modules from ghc-mod
    def init_ghcmod_completions(self):
        # Init LANGUAGE completions
        self.language_completions = call_ghcmod_and_wait(['lang']).split('\n')
        log("Read LANGUAGE completions from ghc-mod")

        # Init import module completion
        self.module_completions = call_ghcmod_and_wait(['list']).split('\n')

    def get_special_completions(self, view, prefix, locations):

        # Contents of the current line up to the cursor
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        # TODO handle multiple selections
        match_language = LANGUAGE_RE.match(line_contents)
        if match_language:
            return [ (unicode(c),) * 2 for c in self.language_completions ]

        # Autocompletion for import statements
        match_import = IMPORT_RE.match(line_contents)
        if match_import:
            import_completions = [ (unicode(c),) * 2 for c in self.module_completions ]

            # Right after "import "? Propose "qualified" as well!
            qualified_match = IMPORT_QUALIFIED_POSSIBLE_RE.match(line_contents)
            if qualified_match:
                qualified_prefix = qualified_match.group('qualifiedprefix')
                if qualified_prefix == "" or "qualified".startswith(qualified_prefix):
                    import_completions.insert(0, (u"qualified", "qualified "))

            return import_completions

        return None

    def on_query_completions(self, view, prefix, locations):
        begin_time = time.clock()
        # Only suggest symbols if the current file is part of a Cabal project.
        # TODO: Only suggest symbols from within this project.

        cabal_dir = get_cabal_project_dir_of_view(view)
        if cabal_dir is not None:

            completions = self.get_special_completions(view, prefix, locations)

            if not completions:
                completions = self.inspector.get_completions(view.file_name())

            end_time = time.clock()
            log('time to get completions: {0} seconds'.format(end_time - begin_time))
            return completions

        return []

    def on_post_save(self, view):
        filename = view.file_name()
        if filename is not None:
            self.inspector.mark_file_dirty(filename)

class InspectorAgent(threading.Thread):
    def __init__(self):
        # Call the superclass constructor:
        super(InspectorAgent, self).__init__()
        # Make this thread daemonic so that it won't prevent the program
        # from exiting.
        self.daemon = True
        # Module info:
        self.info_lock = threading.Lock()
        self.info = {}
        # Files that need to be re-inspected:
        self.dirty_files_lock = threading.Lock()
        self.dirty_files = []

    def run(self):
        # Compile the ModuleInspector:
        exit_code, out, err = call_and_wait(['ghc',
            '--make', MODULE_INSPECTOR_SOURCE_PATH,
            '-o', MODULE_INSPECTOR_EXE_PATH,
            '-outputdir', MODULE_INSPECTOR_OBJ_DIR])
        # TODO: If compilation failed, we can't proceed; handle this.
        # Periodically wake up and see if there is anything to inspect.
        while True:
            files_to_reinspect = []
            with self.dirty_files_lock:
                files_to_reinspect = self.dirty_files
                self.dirty_files = []
            # Find the cabal project corresponding to each "dirty" file:
            cabal_dirs = []
            for filename in files_to_reinspect:
                d = get_cabal_project_dir_of_file(filename)
                if d is not None:
                    cabal_dirs.append(d)
            # Eliminate duplicate project directories:
            cabal_dirs = list(set(cabal_dirs))
            for d in cabal_dirs:
                self._refresh_all_module_info(d)
            time.sleep(AGENT_SLEEP_DURATION)

    def mark_file_dirty(self, filename):
        "Report that a file should be reinspected."
        with self.dirty_files_lock:
            self.dirty_files.append(filename)

    def get_completions(self, current_file_name):
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

    def _refresh_all_module_info(self, cabal_dir):
        "Rebuild module information for all files under the specified directory."
        begin_time = time.clock()
        log('reinspecting project ({0})'.format(cabal_dir))
        # Process all files within the Cabal project:
        # TODO: Only process files within the .cabal file's "src" directory.
        files_in_dir = list_files_in_dir_recursively(cabal_dir)
        haskell_source_files = [x for x in files_in_dir if x.endswith('.hs')]
        for filename in haskell_source_files:
            self._refresh_module_info(filename)
        end_time = time.clock()
        log('total inspection time: {0} seconds'.format(end_time - begin_time))

    def _refresh_module_info(self, filename):
        "Rebuild module information for the specified file."
        # TODO: Only do this within Haskell files in Cabal projects.
        # TODO: Skip this file if it hasn't changed since it was last inspected.
        # If the file hasn't changed since it was last inspected, do nothing:
        modification_time = os.stat(filename).st_mtime
        if CHECK_MTIME:
            inspection_time = self._get_inspection_time_of_file(filename)
            if modification_time <= inspection_time:
                return
        exit_code, stdout, stderr = call_and_wait(
            [MODULE_INSPECTOR_EXE_PATH, filename])
        if exit_code == 0:
            new_info = json.loads(stdout)
        else:
            # There was a problem parsing the file; create an error entry.
            new_info = {'error': 'ModuleInspector failed'}
        # Remember when this info was collected.
        new_info['inspectedAt'] = modification_time
        # Dump the currently-known module info to disk:
        formatted_json = json.dumps(self.info, indent=2)
        with open(OUTPUT_PATH, 'w') as f:
            f.write(formatted_json)
        with self.info_lock:
            self.info[filename] = new_info

    def _get_inspection_time_of_file(self, filename):
        """Return the time that a file was last inspected.
        Return zero if it has never been inspected."""
        with self.info_lock:
            try:
                return self.info[filename]['inspectedAt']
            except KeyError:
                return 0.0

def list_files_in_dir_recursively(base_dir):
    """Return a list of a all files in a directory, recursively.
    The files will be specified by full paths."""
    files = []
    for dirname, dirnames, filenames in os.walk(base_dir):
        for filename in filenames:
            files.append(os.path.join(base_dir, dirname, filename))
    return files
