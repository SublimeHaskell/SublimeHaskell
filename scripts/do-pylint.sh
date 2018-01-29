#!/bin/sh

rcscript="import sys"
if test -d ../SublimeHaskell; then
  rcscript="${rcscript}; sys.path.append('../SublimeHaskell')"
fi
cygpath=echo
if which cygpath > /dev/null 2>&1; then
  cygpath="$(which cygpath) -w"
fi

pkgdir=$(${cygpath} "$(pwd)/..")
for dir in "/Applications/Sublime Text.app/Contents/MacOS" \
	  "$USERPROFILE/Sublime" \
	  "$USERPROFILE/Sublime/Data/Packages"; do
  [ -d "$dir" ] && {
    rcscript="$rcscript; sys.path.append('"$(echo "$dir" | sed -e 's/\\/\\\\/g')"')"
  }
done

pylint --init-hook="$rcscript" -d C0111,R0201,R0903,W0603 *.py internals/*.py hsdev/*.py ghcmod/*.py
# env PYTHONPATH="$pypath:$PYTHONPATH" pylint -d C0111,R0201,R0903,W0603 *.py internals/*.py hsdev/*.py
