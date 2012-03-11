README
======

Requirements
------------
* ghc
* cabal
* Cabal packages: base, bytestring, data-aeson, haskell-src-exts

Installation
------------
1. Get Sublime Text 2 <http://www.sublimetext.com/>
2. Install the Sublime Package Control package <http://wbond.net/sublime_packages/package_control/installation>
3. Use Package Control to install this package (SublimeHaskell)

Usage
-----
When editing Haskell source files that are part of a Cabal project, automatic error highlighting and enhanced auto-completion are available.

Each time you save, any errors in your program will be listed at the bottom of the window and highlighted in the source code.

All source files in the project are scanned when the change. Any symbols that they export are provided in the auto-complete suggestions.
