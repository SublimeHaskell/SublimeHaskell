#!/bin/sh

rcscript="import sys"
cygpath=echo
which cygpath > /dev/null 2>&1 && cygpath="$(which cygpath) -w"
for dir in $(${cygpath} `pwd`/..) \
          '/Applications/Sublime Text.app/Contents/MacOS' \
	  "$USERPROFILE\\Sublime" \
	  "$USERPROFILE\\Sublime\\Data\\Packages"; do
  [ -d "$dir" ] && {
    rcscript="$rcscript; sys.path.append('"$(echo "$dir" | sed -e 's/\\/\\\\/g')"')"
  }
done

pylint --init-hook="$rcscript" -d C0111,R0201,R0903,W0603 *.py internals/*.py hsdev/*.py
# env PYTHONPATH="$pypath:$PYTHONPATH" pylint -d C0111,R0201,R0903,W0603 *.py internals/*.py hsdev/*.py
