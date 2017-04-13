SublimeHaskell Editing Guide
============================

<!-- MarkdownTOC -->

- Commands
    - `stack exec `
- Snippets
- Key Bindings
    - Comments
- Customizing The Build System
    - cabal
    - hdevtools
    - Using build system results
- Using other useful projects with SublimeText
    - Jump-to-definition
    - Inside your project: hasktags

<!-- /MarkdownTOC -->


## Commands

These are the _SublimeHaskell_ commands accessible via the command pane (&lt;ctrl&gt;-&lt;shift&gt;-P on Windows and Linux, &lt;cmd&gt;-&lt;shift&gt;-P on Mac). Commands are prefixed by "SublimeHaskell:".

| Command                             | Descr        |
| ----------------------------------- | ------------ |
| ...: Apply to selection             |              |
| ...: Apply to selection list        |              |
| ...: Auto fix                       |              |
| ...: Browse declarations            |              |
| ...: Browse module                  |              |
| ...: Build                          |              |
| ...: Cabal: List                    |              |
| ...: Check                          | Send the buffer's contents to the attached backend and check syntax and type consistency. The error window reports all errors and warnings found. |
| ...: Check & Lint                   | Conducts a `...: Check` and, if the Haskell source has no errors, also passes the source through `hlint` to suggest potential improvements. |
| ...: Clean                          | Invokes the build system's "clean" command. |
| ...: Configure                      | For the `cabal` builder only: (re-)configures the project associated with the currently active buffer. |
| ...: Eval selection                 |              |
| ...: Expand selection to Expression |              |
| ...: Find declarations              |              |
| ...: Go to declaration              |              |
| ...: Go to module                   |              |
| ...: Go to next error               |              |
| ...: Go to previous error           |              |
| ...: Hayoo                          |              |
| ...: HIndent                        | Invokes the `hindent` indenter on the selected region, or the entire buffer if no selected region. |
| ...: Insert import for symbol       |              |
| ...: Insert type                    |              |
| ...: Install                        |              |
| ...: Lint                           |              |
| ...: Rebuild                        | Invoke the build system to `clean` followed by a `build`, e.g., `cabal clean` followed by `cabal build`. |
| ...: Reinspect all                  | Rescan the current windows, collect their respective `stack` or `cabal` projects, and scan projects' packages and source files for symbols. Used to seed and generate autocompletion lists. |
| ...: Run                            |              |
| ...: Scan docs and infer types      |              |
| ...: Search declarations everywhere |              |
| ...: Show error panel               |              |
| ...: Show symbol info               |              |
| ...: Show type                      |              |
| ...: Show types                     |              |
| ...: Show/hide all types            |              |
| ...: Stack Exec                     | Prompt for a command to execute as `stack exec <command>` and display its output in the `build` output window. |
| ...: Stylish                        | Invokes the `stylish-haskell` indenter on the selected region, or the entire buffer if no selected region. |
| ...: Test                           |              |
| ...: Toggle symbol info panel       |              |
| ...: Typecheck build (no codegen)   |              |

### `stack exec <command>`

`--` is inserted into the command list if `<command>` contains options (arguments that start with `-`) and you haven't specified `--` already. This comes in handy if you enter the following line into the `stack exec` command prompt:

```shell
-no-ghc-packages -- cabal --help
```

If you include the `--`, the `stack exec` command will leave the command list alone. However, if you simply type:

```shell
cabal --help
```

then `stack exec` inserts a `--` at the beginning of the command, e.g., `stack exec -- cabal --help`. If `stack exec` didn't, you would end up seeing the output of `stack exec`'s help because `stack exec` interprets options before `--` as its own.

## Snippets

Snippets are a way to quickly insert code by typing all or part of the snippet trigger and pressing `TAB`. For example, typing `data<TAB>` inserts the snippet for a plain (not GADT) data type:

```Haskell
data Type = Type  deriving (Eq, Show)
```

| Snippet trigger | What it inserts                                                                           |
| --------------- | ----------------------------------------------------------------------------------------- |
| `data`          | Data type and constructor, where the `Type` and the first constructor have the same name. |
| `impq`          | Double import: Two import lines, one unqualified and the other unqualified.               |
| `if`            | Inserts an `if` expression.                                                               |
| `inline`        | `{-# INLINE function #-}` pragma.                                                         |
| `lang`          | Inserts a GHC `{-# LANGUAGE pragma #-}` pragma.                                           |
| `newtype`       | Inserts a `newtype` declaration.                                                          |
| `opts`          | Inserts a GHC `{-# OPTIONS_GHC options #-}` option comment.                               |
| `qualified`     | Inserts a qualified import.                                                               |
| `record`        | Record data type and constructor.                                                         |

Double import snippet example:
```Haskell
import           Data.Text (Text, fun)
import qualified Data.Text as T
```

Record snippet example:
```Haskell
data Record = Record
  { field :: Type
  } deriving (Eq, Show)
```

## Key Bindings

### Comments

| Key            | Action                                 |
| -------------- | -------------------------------------- |
| `Ctrl-/`       | Inserts a Haskell line comment `-- `   |
| `Ctrl-Shift-/` | Inserts a Haskell block comment `{--}` |

## Customizing The Build System

You don't have to use SublimeHaskell's built-in build functionality. If you prefer, you can disable them in the settings, and use the plain Sublime Build Systems:

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

## Using other useful projects with SublimeText

### Jump-to-definition

There are two kinds of jump-to-definition: Inside your project and outside your project.
In any case, install the Sublime [`CTags`](https://github.com/SublimeText/CTags) package via Package Control,
and `cabal install hasktags`.

`CTags` expects the extended exuberant ctags format.

### Inside your project: hasktags

* In your project, `hasktags --ctags --extendedctag .`
* You can now jump to definitions inside your project (`Ctrl-T, Ctrl-T` is the default keybinding)
