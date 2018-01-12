'''Regular expressions used across modules -- in this module to prevent circular dependencies.
'''

import re

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# It also eats whitespace before the first line.
# The first line is divided into a filename, a line number, and a column.
GHC_DIAGNOSTIC_REGEX = re.compile(r'\s*^(\S*):(\d+):(\d+):(.*$(?:\n^(?:[ \t]+|\d+ \|).*$)*)', re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
RESULT_FILE_REGEX = r'^\s{2}(\S*?): line (\d+), column (\d+):$'

LANGUAGE_RE = re.compile(r'{-#\s+LANGUAGE.*')
'''Regular expression for GHC "LANGUAGE" pragmas.
'''

OPTIONS_GHC_RE = re.compile(r'{-#\s+OPTIONS_GHC.*')
