README
======

Requirements
------------

Necessary:
* A recent ghc (7.10.x)
* cabal 1.18 or later

Optional, but useful:
* [ghc-mod](http://hackage.haskell.org/package/ghc-mod) (for import and LANGUAGE completions and type inference, `cabal install ghc-mod`)
* [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) (for code prettification, `cabal install stylish-haskell`)
* [cabal-dev](http://hackage.haskell.org/package/cabal-dev) if you want to use it
* [haskell-docs](http://hackage.haskell.org/package/haskell-docs) (for documentation in 'Symbol info' command, `cabal install haskell-docs`)
* [hdevtools](https://github.com/bitc/hdevtools) (or [fork for windows](https://github.com/mvoidex/hdevtools)) (for type inference, `cabal install hdevtools`)

Binaries:
* If your `cabal`, `ghc-mod`, `ghc` etc. are not installed in a system PATH, you have to adjust SublimeHaskell's `add_to_PATH` setting.

Installation
------------
1. Get Sublime Text 2: <http://www.sublimetext.com/>
2. Install the Sublime Package Control package: <http://wbond.net/sublime_packages/package_control/installation>
3. Use Package Control to install this package (SublimeHaskell)

Usage
-----
In short: Press `Shift-Ctrl-P` and type `haskell` to explore all commands.

When editing Haskell source files that are part of a Cabal project, automatic error highlighting and enhanced auto-completion are available.

Each time you save, any errors in your program will be listed at the bottom of the window and highlighted in the source code.

All source files in the project are scanned when the change. Any symbols that they export are provided in the auto-complete suggestions.

To use cabal-dev instead of cabal, set use_cabal_dev to true (or use command "Switch Cabal/Cabal-Dev") and specify cabal-dev absolute path. Completion list will be rescanned and build will use cabal-dev.

Stylish-haskell can be used to stylish file or selected text.

Use `Ctrl-Shift-R` to go to declaration and `Ctrl-K-I` to show symbol info with documentation. These command are also available through context menu with right-click.

Command 'SublimeHaskell: Browse module' is similar to ghci's browse command

To show inferred types use `Show type` (`ctrl-k ctrl-h ctrl-t`) command.

To insert inferred type use `Insert type` (`ctrl-k ctrl-h ctrl-i`).

You can jump between the errors and warnings with `F4` and `Shift-F4`.
To show hidden error output, use command `Show error panel` (`ctrl-alt-e`)

Build Systems
-------------

You don't have to use SublimeHaskell's built-in build functionality.

If you prefer, you can disable them in the settings, and use plain Sublime Build Systems:

### cabal

Save this to your `~/.config/sublime-text-2/Packages/User/cabal-custom.sublime-build` to make a custom `cabal` build system:

```json
{
  "cmd": ["cabal build --ghc-options='-O0 -hidir o0 -odir o0'"],  // append lib:myPackage or myexecutable here to only build certain cabal targets
  "shell": true,
  "file_regex": "^(\\S*?):(\\d+):(\\d+):$"  // this matches the output of ghc
}
```

For more options, [look here](http://docs.sublimetext.info/en/latest/reference/build_systems.html).

### hdevtools

Save this to your `~/.config/sublime-text-2/Packages/User/hdevtools.sublime-build` to make `hdevtools` a build system:

```json
{
  "cmd": ["/home/USERNAME/.cabal/bin/hdevtools", "check", "-g", "-Wall", "$file"],
  "file_regex": "^(.*?):(\\d+):(\\d+):",
  "selector": "source.haskell"
}
```

### Using build system results

You can then build with `Ctrl-B` and jump between the errors with (Shift-)`F4`.

It is also useful to add this to your key bindings to redisplay the error panel at any time:

```json
  { "keys": ["ctrl+alt+b"], "command": "show_panel", "args": {"panel": "output.exec"} }
```

Using other useful projects with SublimeText
--------------------------------------------

### Jump-to-definition

There are two kinds of jump-to-definition: Inside your project and outside your project.
In any case, install the Sublime [`CTags`](https://github.com/SublimeText/CTags) package via Package Control,
and `cabal install hasktags`.

`CTags` expects the extended exuberant ctags format.

#### Outside your project: hasktags

* In your project, `hasktags --ctags --extendedctag .`
* You can now jump to definitions inside your project (`Ctrl-R, Ctrl-R` is the default keybinding)

#### Outside your project: codex

[codex](https://hackage.haskell.org/package/codex) allows you to use ctags to jump to definitions that are declared in your cabal dependencies.

* `cabal install codex`
* In your project, `codex update`
* Change `~/.codex` to `tagsCmd: hasktags --ctags --extendedctag --output='$TAGS' '$SOURCES'`
* In your project, `codex cache clean && codex update`
* In the Sublime CTags user settings, set `"tag_file": "codex.tags"`
* You can now jump to the source code of definitions outside of your project.


If the ModuleInspector takes too much time
------------------------------------------

The `ModuleInspector` is a program that looks at your Haskell environment to provide auto completion.

Depending on your environment, this may take very long.

You can disable it with the `"inspect_modules": false` setting.
