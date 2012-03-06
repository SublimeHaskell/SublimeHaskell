import re
import sublime
import sublime_plugin

# Completion text longer than this is ellipsized:
MAX_COMPLETION_LENGTH = 37

# Match a module's name, not followed by an export list:
module_name_regex = re.compile(
    r'^module\w+([\w.])+\s+where',
    re.DOTALL | re.MULTILINE)

# Match a module's name and its explicit export list:
export_list_regex = re.compile(
    r'^module\w+([\w.])+\s*\((.*)\)\s*where',
    re.DOTALL | re.MULTILINE)

# Match a function declaration's name and type:
# TODO: This only matches type declarations that fit on one line.
function_declaration_regex = re.compile(
    r'^([\w\']+)\s*::\s*(.+?)\s*$',
    re.MULTILINE)

# Match a type declaration's name and type:
# TODO: This does not capture type parameters or type constructors.
type_declaration_regex = re.compile(
    r'^(data|type|newtype)\s+([\w\']+)',
    re.MULTILINE)

class SublimeHaskellAutocomplete(sublime_plugin.EventListener):
    def on_query_completions(self, view, prefix, locations):
        # TODO: Only do this within Haskell files in Cabal projects.
        info = analyze_file(view.file_name())
        completions = [(str(x), x.name) for x in info.declarations]
        return completions

class ModuleCompletionInfo(object):
    """Describe a file's module name, imports, exports, and declarations.
    name :: String or None -- name of the module, if present
    export_list :: [String] or None -- list of explicit exports, if present
    import_list :: [String] -- list of imported modules
    declarations :: [(String, String)] -- names and descriptions of 
        declarations in the file
    """
    def __init__(self, name, export_list, import_list, declarations):
        self.name = name
        self.export_list = export_list
        self.import_list = import_list
        self.declarations = declarations

class ModuleDefinition(object):
    """Describe a single type or function definition.
    """
    def __init__(self, name, type_info):
        self.name = name
        self.type_info = type_info

    def __str__(self):
        s = '{0} :: {1}'.format(self.name, self.type_info)
        return s[:MAX_COMPLETION_LENGTH]

def analyze_file(filename):
    "Extract ModuleCompletionInfo from a file."
    content = ''
    with open(filename) as f:
        content = f.read()
    # Find the declared functions:
    matches = function_declaration_regex.finditer(content)
    functions = []
    for m in matches:
        name, type_info = m.groups()
        functions.append(ModuleDefinition(name, type_info))
    return ModuleCompletionInfo('Unknown', [], [], functions)
