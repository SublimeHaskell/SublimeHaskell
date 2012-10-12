import json
import os
import re
import sublime
import sublime_plugin
import subprocess
import threading
import time

from sublime_haskell_common import PACKAGE_PATH, get_setting, get_setting_async, get_cabal_project_dir_of_file, get_cabal_project_dir_of_view, call_and_wait, call_ghcmod_and_wait, log, wait_for_window, output_error, get_settings, attach_sandbox

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

# Checks if a word contains only alhanums, -, and _
NO_SPECIAL_CHARS_RE = re.compile(r'^(\w|[\-])*$')


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
        self.module_completions = []

        self.local_settings = {
            'enable_ghc_mod' : None,
            'use_cabal_dev' : None,
            'cabal_dev_sandbox' : None }

        for s in self.local_settings.keys():
            self.local_settings[s] = get_setting(s)

        self.init_ghcmod_completions()

        # Subscribe to settings changes to update data
        get_settings().add_on_change('enable_ghc_mod', lambda: self.on_setting_changed())

    def on_setting_changed(self):
        same = True
        for k, v in self.local_settings.items():
            r = get_setting(k)
            same = same and v == r
            self.local_settings[k] = r

        if not same:
            self.init_ghcmod_completions()

    # Gets available LANGUAGE options and import modules from ghc-mod
    def init_ghcmod_completions(self):

        if not get_setting('enable_ghc_mod'):
            return

        sublime.status_message('SublimeHaskell: Updating ghc_mod completions...')

        # Init LANGUAGE completions
        self.language_completions = call_ghcmod_and_wait(['lang']).splitlines()
        log("Reading LANGUAGE completions from ghc-mod")

        # Init import module completion
        self.module_completions = call_ghcmod_and_wait(['list']).splitlines()

        sublime.status_message('SublimeHaskell: Updating ghc_mod completions ' + u" \u2714")

    def get_special_completions(self, view, prefix, locations):

        # Contents of the current line up to the cursor
        line_contents = get_line_contents(view, locations[0])

        # Autocompletion for LANGUAGE pragmas
        if get_setting('auto_complete_language_pragmas'):
            # TODO handle multiple selections
            match_language = LANGUAGE_RE.match(line_contents)
            if match_language:
                return [ (unicode(c),) * 2 for c in self.language_completions ]

        # Autocompletion for import statements
        if get_setting('auto_complete_imports'):
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
        # if cabal_dir is not None:

        completions = self.get_special_completions(view, prefix, locations)

        if not completions:
            completions = self.inspector.get_completions(view.file_name())

        end_time = time.clock()
        log('time to get completions: {0} seconds'.format(end_time - begin_time))
        # Don't put completions with special characters (?, !, ==, etc.)
        # into completion because that wipes all default Sublime completions:
        # See http://www.sublimetext.com/forum/viewtopic.php?t=8659
        # TODO: work around this
        return [ c for c in completions if NO_SPECIAL_CHARS_RE.match(c[0]) ]

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
        # Module info (dictionary: filename => info):
        self.info_lock = threading.Lock()
        self.info = {}
        # Standard module completions (dictionary: module name => completions):
        self.std_info_lock = threading.Lock()
        self.std_info = {}
        # Files that need to be re-inspected:
        self.dirty_files_lock = threading.Lock()
        self.dirty_files = []

    def run(self):
        # Compile the ModuleInspector:
        sublime.set_timeout(lambda: sublime.status_message('Compiling Haskell ModuleInspector...'), 0)

        exit_code, out, err = call_and_wait(['ghc',
            '--make', MODULE_INSPECTOR_SOURCE_PATH,
            '-o', MODULE_INSPECTOR_EXE_PATH,
            '-outputdir', MODULE_INSPECTOR_OBJ_DIR])

        if exit_code != 0:
            error_msg = u"SublimeHaskell: Failed to compile ModuleInspector\n{0}".format(err)
            wait_for_window(lambda w: self.show_errors(w, error_msg))
            return

        sublime.set_timeout(lambda: sublime.status_message('Compiling Haskell ModuleInspector' + u" \u2714"), 0)

        # TODO: If compilation failed, we can't proceed; handle this.
        # Periodically wake up and see if there is anything to inspect.
        while True:
            files_to_reinspect = []
            with self.dirty_files_lock:
                files_to_reinspect = self.dirty_files
                self.dirty_files = []
            # Find the cabal project corresponding to each "dirty" file:
            cabal_dirs = []
            standalone_files = []
            for filename in files_to_reinspect:
                d = get_cabal_project_dir_of_file(filename)
                if d is not None:
                    cabal_dirs.append(d)
                else:
                    standalone_files.append(filename)
            # Eliminate duplicate project directories:
            cabal_dirs = list(set(cabal_dirs))
            standalone_files = list(set(standalone_files))
            for d in cabal_dirs:
                self._refresh_all_module_info(d)
            for f in standalone_files:
                self._refresh_module_info(f)
            time.sleep(AGENT_SLEEP_DURATION)

    def show_errors(self, window, error_text):
        sublime.set_timeout(lambda: sublime.status_message('Compiling Haskell ModuleInspector' + u" \u2717"), 0)
        sublime.set_timeout(lambda: output_error(window, error_text), 0)

    def mark_file_dirty(self, filename):
        "Report that a file should be reinspected."
        with self.dirty_files_lock:
            self.dirty_files.append(filename)

    def get_completions(self, current_file_name):
        "Get all the completions that apply to the current file."
        # TODO: Filter according to what names the current file has in scope.

        completions = []

        with self.info_lock:
            # File not processed yet
            if current_file_name not in self.info:
                return

            moduleImports = self.info[current_file_name]['importList']

            for file_name, file_info in self.info.items():
                if 'error' in file_info:
                    # There was an error parsing this file; skip it
                    continue

                # File is imported, add to completion list
                if file_info['moduleName'] in moduleImports:
                    for d in file_info['declarations']:
                        identifier = d['identifier']
                        declaration_info = d['info']
                        # TODO: Show the declaration info somewhere.
                        completions.append((identifier[:MAX_COMPLETION_LENGTH], identifier))

            # Completion for modules by ghc-mod browse
            with self.std_info_lock:
                for mi in moduleImports:
                    if mi not in self.std_info:
                        # Module not imported, skip it
                        continue

                    std_module = self.std_info[mi]

                    for v in std_module:
                        completions.append((v[:MAX_COMPLETION_LENGTH], v))

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
        # TODO: Currently the ModuleInspector only delivers top-level functions
        #       with hand-written type signatures. This code should make that clear.
        # If the file hasn't changed since it was last inspected, do nothing:
        if not filename.endswith('.hs'):
            return

        modification_time = os.stat(filename).st_mtime
        if CHECK_MTIME:
            inspection_time = self._get_inspection_time_of_file(filename)
            if modification_time <= inspection_time:
                return
        exit_code, stdout, stderr = call_and_wait(
            [MODULE_INSPECTOR_EXE_PATH, filename])
        if exit_code == 0:
            new_info = json.loads(stdout)
            # Load standard modules
            if 'importList' in new_info:
                for mi in new_info['importList']:
                    self._load_standard_module(mi)
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

    def _load_standard_module(self, module_name):
        if module_name not in self.std_info:
            module_contents = call_ghcmod_and_wait(['browse', module_name]).splitlines()
            with self.std_info_lock:
                self.std_info[module_name] = module_contents

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
