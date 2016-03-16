README
======

![look](Themes/info+repl.png)

Requirements
------------

Necessary:
* ghc and a recent Haskell Platform (>= 2012 should do fine)
* cabal

Not necessary, but useful
* [hsdev](http://hackage.haskell.org/package/hsdev) cabal package (`cabal install hsdev`) for inspection, enhanced completion, goto, symbol info etc.
* [SublimeREPL](https://github.com/wuub/SublimeREPL) package for repl commands
* [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) (for code prettification, `cabal install stylish-haskell`)
* [ghc-mod](http://hackage.haskell.org/package/ghc-mod) (for import and LANGUAGE completions and type inference, `cabal install ghc-mod`, not used if `hsdev` enabled)
* [haskell-docs](http://hackage.haskell.org/package/haskell-docs) (for documentation in 'Symbol info' command, `cabal install haskell-docs`, not used if `hsdev` enabled)
* [hdevtools](https://github.com/bitc/hdevtools) (or [fork for windows](https://github.com/mvoidex/hdevtools)) (for type inference, `cabal install hdevtools`, not used if `hsdev` enabled)

Binaries:
* If your `cabal`, `ghc-mod`, `ghc` etc. are not installed in a system PATH, you have to adjust SublimeHaskell's `add_to_PATH` setting.

There are also [special theme](Themes/Hasky%20\(Dark\).gif) with enhanced haskell entities coloring<br>
Note different coloring for types and constructors (in import list, data declaration etc.), special coloring of generic variables in types, pragmas and module imports

![compare](Themes/Hasky%20\(Dark\).small.gif)

Installation
------------

1. Get Sublime Text 2/3: <http://www.sublimetext.com/>
2. Install the Sublime Package Control package: <http://wbond.net/sublime_packages/package_control/installation>
3. Use Package Control to install this package (SublimeHaskell)

Usage
-----

In short: Press `shift-ctrl-p` and type `haskell` to explore all commands.

When editing Haskell source files, automatic error highlighting and enhanced auto-completion is available.

Each time you save, any errors in your program will be listed at the bottom of the window and highlighted in the source code.

All source files in the project are scanned on change. Any symbols that they export are provided in the auto-complete suggestions.

Stylish-haskell can be used to stylish file or selected text.

Use `f12` to go to declaration and `ctrl+k ctrl+i` to show symbol info with documentation. These command are also available through context menu with right-click.

To show inferred types use `Show type` (`ctrl-k ctrl-h ctrl-t`) command.

To insert inferred type use `Insert type` (`ctrl-k ctrl-h ctrl-i`).

You can jump between the errors and warnings with `alt+d alt+e` and `alt+shift+d alt+shift+e`.
To show hidden error output, use command `Show error panel` (`ctrl-alt-e`)

Enhanced completion:
![Autocompletion](Commands/Autocompletion.gif)

Commands:
---

* `SublimeHaskell: Insert import for symbol` — add import for declaration
![InsertImport](Commands/InsertImport.gif)
* `SublimeHaskell: Find declarations` — find declarations in installed packages and in projects
* `SublimeHaskell: Search declarations everywhere` — search declarations in hayoo too
* `SublimeHaskell: Browse module` — get declarations for module
![BrowseModule](Commands/BrowseModule.gif)
* `SublimeHaskell: Browse declarations` — get declarations in scope of current file
* `SublimeHaskell: Show symbol info` — get help for symbol, `ctrl+k ctrl+i`
![SymbolInfo](Commands/SymbolInfo.gif)
* `SublimeHaskell: Toggle symbol info panel` — toggle continuout symbol info panel, which show info about symbol under cursor. Useful when reading code.
![ToggleSymbolInfo](Commands/ToggleSymbolInfo.gif)
* `SublimeHaskell: Go to module` — go to module, `ctrl+k ctrl+p`
![GoToModule](Commands/GoToModule.gif)
* `SublimeHaskell: Go to declaration` — overrides default, `f12`
* `Ctrl+R`, `Ctrl+Shift+R` — overrides default, goto symbol and goto symbol in project
* `SublimeHaskell: Show type` — show type/types for current expression, `ctrl-k ctrl-h ctrl-t`
* `SublimeHaskell: Expand selection to Expression` — expand selection to expression, `ctrl+shift+y`
* `SublimeHaskell: Show/hide all types` — get all types and highlight while selection modifies, `ctrl+t, h`
![Types](Commands/Types.gif)
* `SublimeHaskell: Insert type` — insert type for selected expression, `ctrl-k ctrl-h ctrl-i`
* `SublimeHaskell: Hayoo` — search in hayoo
* `SublimeHaskell: Auto fix` — auto fix some of warnings and/or errors (for now redundant imports and hlint hints)
![AutoFix](Commands/AutoFix.gif)
* `SublimeHaskell: Stylish` — stylish source with `stylish-haskell`
* `SublimeHaskell: Scan docs and infer types` — as long as scanning docs for sources and inferring types is long process, it's disabled by default, but this command can be used to scan docs and infer types for currently opened view
* `SublimeHaskell: Check & Lint` — check/lint/check & lint opened file. Enable option `check_lint_fly` to [check & lint](Commands/FlyCheck.gif) on idle, rescanning actual source, so that [completions are updated](Commands/ScanContents.gif)
* Eval commands — see [animation](Commands/Eval.gif)
  * `SublimeHaskell: Eval selection` — eval selected expression, for example
    * `[1..10]` ⤇ `[1,2,3,4,5,6,7,8,9,10]`
    * `replicate 10 'a'` ⤇ `aaaaaaaaaa` (note no double quotes for string result)
  * `SublimeHaskell: Apply to selection` — same as above, but applies function to each selection
    * `foobar` ⫤ `reverse` ⤇ `raboof`
    * `[1..10]` ⫤ `reverse` ⤇ `[10,9,8,7,6,5,4,3,2,1]`
    * `1`, `2`, `3` ⫤ `succ` ⤇ `2`, `3`, `4`
    * `[1..3]` ⫤ `intercalate ", " . map (\i -> "foo" ++ show i)` ⤇ `foo1, foo2, foo3`
  * `SublimeHaskell: Apply to selection list` — applies function to list made from selections
    * `foo`, `bar`, `baz` ⫤ `reverse` ⤇ `baz`, `bar`, `foo`
    * `foo`, `bar`, `baz` ⫤ `sort` ⤇ `bar`, `baz`, `foo`
* Repl commands (uses [SublimeREPL](https://github.com/wuub/SublimeREPL) package)
  * `SublimeHaskell Repl: GHCi` — runs `ghci`
  * `SublimeHaskell Repl: GHCi current file` — runs `ghci` and loads current file
  * `SublimeHaskell Repl: Cabal Repl` — runs `cabal repl` for current project
  * `SublimeHaskell Repl: Load` — loads current file or project in repl
* Context menu commands
  * `Open package on Hackage` — works within symbol info panel, opens Hackage page
  * `Open module on Hackage` — words in symbol info panel and in sources, opens Hackage page for selected module
* Build commands
  * `Build`, `Typecheck build (no codegen)`, `Clean`, `Configure`, `Rebuild`, `Install`, `Test`, `Run`
* Error commands:
  * `SublimeHaskell: Go to next error` — go to next error in file, `alt+d alt+e`
  * `SublimeHaskell: Go to previous error` — go to previous error in file, `alt+shift+d alt+shift+e`
  * `SublimeHaskell: Show error panel` — show error panel if it was hidden, `ctrl+alt+e`

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

#### Inside your project: hasktags

* In your project, `hasktags --ctags --extendedctag .`
* You can now jump to definitions inside your project (`Ctrl-T, Ctrl-T` is the default keybinding)

#### Inside and outside your project: codex

[codex](https://hackage.haskell.org/package/codex) allows you to use ctags to jump to definitions that are declared in your cabal dependencies.

* `cabal install codex`
* Run `codex set format sublime`, that updates your `~/.codex` file to Sublime's Ctags plugin's format
* Change `~/.codex` to `tagsFileName: .tags`
* In your project, `codex cache clean && codex update`
* You can now jump to the source code of definitions outside of your project.
* The commands `CTags: Show Symbols` and `CTags: Rebuild Tags` currently don't work with `codex`
