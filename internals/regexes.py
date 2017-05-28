'''Regular expressions used across modules -- in this module to prevent circular dependencies.
'''

import re

# This regex matches an unindented line, followed by zero or more
# indented, non-empty lines.
# It also eats whitespace before the first line.
# The first line is divided into a filename, a line number, and a column.
OUTPUT_REGEX = re.compile(r'\s*^(\S*):(\d+):(\d+):(.*$(?:\n^[ \t].*$)*)', re.MULTILINE)
GHC_OUTPUT_REGEX = re.compile(r'\s*^(?P<file>\S*):(?P<line>\d+):(?P<col>\d+):(?P<flag>[Ww]arning:\s+)?(.*$(?:\n^[ \t].*$)*)',
							                       re.MULTILINE)

# Extract the filename, line, column, and description from an error message:
RESULT_FILE_REGEX = r'^\s{2}(\S*?): line (\d+), column (\d+):$'
