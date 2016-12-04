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

class Myclass a
  -- Class before where comment (PASS)
  where

myFunction1
  :: Int -- Type signature on next line (PASS)
  -> String
myFunction1 = undefined

myFunction2 :: Int -> String -- Type signature (PASS)
myFunction2 = undefined

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

keys'Mb -- should not detect `Mb` as a constructor ("constant") (PASS)
  = undefined

data D where{A :: Int -> D; B :: D } -- A should be highlighted (PASS)


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
