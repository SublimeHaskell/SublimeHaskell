-- SYNTAX TEST "syntax_test_haskell.hs"
{-# LANGUAGE GADTs #-}

module Arrays (
  -- Export list comment (PASS)
  Type(..)
) where

import Data.List (sort, nub) -- import comment (PASS)

-- Normal comment (PASS)
--Normal comment missing space (PASS)

data Type = Type Int deriving
  (Eq, Show) -- deriving on next line (PASS)

a = "asdf"
b = 2

newtype Fribbles x = Fribbles x

class Myclass a
  -- Class before where comment (PASS)
  where

myFunction1
  :: Int -- Type signature on next line (PASS)
  -> String
myFunction1 = undefined

myFunction2 :: Int -> String -- Type signature (PASS)
myFunction2 = undefined

myFunction2a :: Fribbles a -> ()
myFunction2a a = undefined

myFunction2b :: (Myclass a) => Fribbles a -> ()
myFunction2b a = undefined

myFunction3 :: Int            -- Multiline type signature (PASS)
            -> String
myFunction3 = undefined

myFunction4 :: (Myclass a) => -- Multiline type signature w/constraint (PASS)
               Fribbles a
            -> String
myFunction4 _v =
  let
      animaniac = 0
  in  show animaniac

mymodule -- function containing "module" (PASS)
  = undefined

myimport -- function containing "import" (PASS)
  = undefined


-- Multiline comments with pragmas in the middle:
{-
{-# INLINE foo #-}
foo :: Int -> Int
foo x = x
-} -- should be a comment (PASS)

c = 1

-- Multi-line strings
x = "this is my\
    \a\
    \string"
    -- The `\a` should not be highlighted (PASS)


class Foo a where
  foo :: Int -> a

instance Foo Int where
  foo 2 = 3
  foo _ = 4

class (Eq a) => Bar a where
  fob :: Int -> a


keys'Mb -- should not detect `Mb` as a constructor ("constant") (PASS)
  = undefined

data D where{A :: Int -> D; B :: D } -- A should be highlighted (PASS)


{- Multiline block comment.

   Last line has an -- inline comment indicator -}

data MyCTor = MyCtor


-- The following things shouldn't be comments but operators (PASS):
_ --! _ = undefined
_ --# _ = undefined
_ --$ _ = undefined
_ --% _ = undefined
_ --& _ = undefined
_ --* _ = undefined
_ --+ _ = undefined
_ --. _ = undefined
_ --/ _ = undefined
_ --< _ = undefined
_ --= _ = undefined
_ --> _ = undefined
_ --? _ = undefined
_ --@ _ = undefined
_ --\ _ = undefined
_ --^ _ = undefined
_ --| _ = undefined
_ --~ _ = undefined
_ --: _ = undefined
_ --⋆ _ = undefined

-- The following should be comments (PASS):
-- a
--- a
---- a
--_ _ = undefined
--" _ = undefined
--' _ = undefined

--( _ = undefined
--) _ = undefined
--, _ = undefined
--; _ = undefined
--[ _ = undefined
--] _ = undefined
--` _ = undefined
--{ _ = undefined
--} _ = undefined

guarded_func x
  | x == 0
  = True
  | x < 0
  = False
  | otherwise
  = False
