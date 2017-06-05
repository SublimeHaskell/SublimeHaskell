#!/bin/sh

rcscript="import sys"
for dir in `pwd`/.. \
          '/Applications/Sublime Text.app/Contents/MacOS'; do
  [ -d "$dir" ] && rcscript="$rcscript; sys.path.append('$dir')"
done

pylint --init-hook="$rcscript" -d C0111,R0201,R0903,W0603 *.py internals/*.py hsdev/*.py
# env PYTHONPATH="$pypath:$PYTHONPATH" pylint -d C0111,R0201,R0903,W0603 *.py internals/*.py hsdev/*.py
