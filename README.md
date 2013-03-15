README
======

Requirements
------------

Necessary:
* ghc and a recent Haskell Platform (>= 2012 should do fine)
* cabal
* Cabal packages: base, bytestring, aeson, haskell-src-exts, haddock (`cabal install aeson haskell-src-exts haddock`)

Optional, but useful:
* ghc-mod (for import and LANGUAGE completions and type inference, `cabal install ghc-mod`)
* stylish-haskell (for code prettification, `cabal install stylish-haskell`)
* cabal-dev if you want to use it
* haskell_docs

Installation
------------
1. Get Sublime Text 2: <http://www.sublimetext.com/>
2. Install the Sublime Package Control package: <http://wbond.net/sublime_packages/package_control/installation>
3. Use Package Control to install this package (SublimeHaskell)

Usage
-----
When editing Haskell source files that are part of a Cabal project, automatic error highlighting and enhanced auto-completion are available.

Each time you save, any errors in your program will be listed at the bottom of the window and highlighted in the source code.

All source files in the project are scanned when the change. Any symbols that they export are provided in the auto-complete suggestions.

To use cabal-dev instead of cabal, set use_cabal_dev to true (or use command "Switch Cabal/Cabal-Dev") and specify cabal-dev absolute path. Completion list will be rescanned and build will use cabal-dev.

Stylish-haskell can be used to stylish file or selected text.

Use Ctrl-Shift-R to go to declaration and Ctrl-K-I to show symbol info with documentation

Command 'SublimeHaskell: Browse module' is similar to ghci's browse command

Keybindings
-----------
You can add key bindings for type inference to `Key Bindings - User`:

```json
    {
        "command": "haskell_show_type",
        "context": "source.haskell",
        "keys": ["ctrl+k", "ctrl+h", "ctrl+t"]
    },
    {
        "command": "haskell_insert_type",
        "context": "source.haskell",
        "keys": ["ctrl+k", "ctrl+h", "ctrl+i"]
    }
```
