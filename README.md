README
======

Requirements
------------

Necessary:
* ghc and a recent Haskell Platform (>= 2012 should do fine)
* cabal
* Cabal packages: base, bytestring, aeson, haskell-src-exts (== 1.14.*), haddock (`cabal install aeson haskell-src-exts haddock`)
* If you are using GHC 7.6, you might have trouble with too new versions of haddock; in that case, try `cabal install haddock --constraint=haddock==2.13.2.1`

Optional, but useful:
* [ghc-mod](http://hackage.haskell.org/package/ghc-mod) (for import and LANGUAGE completions and type inference, `cabal install ghc-mod`)
* [stylish-haskell](https://github.com/jasper
