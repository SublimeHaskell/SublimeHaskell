SublimeHaskell Editing Guide
============================

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

Double import snippet:
```Haskell
import           Data.Text (Text, fun)
import qualified Data.Text as T
```

Record snippet:
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
