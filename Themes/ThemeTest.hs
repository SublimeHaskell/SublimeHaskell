{-# LANGUAGE UnicodeSyntax, LambdaCase, TemplateHaskell, ForeignFunctionInterface, CPP, GADTs, DatatypeContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- punctuation.pragma.haskell, pragma.name.haskell, pragma.support.language.haskell, pragma.support.flag.haskell, pragma.haskell

-- keyword.module.haskell, entity.name.module.haskell, entity.name.data.haskell, entity.name.class.haskell
module ThemeTest (
	Dat(..), Foo, ClassTest(..),
	fun, fun2, fun3,
	-- entity.name.function.haskell
	(.+.), (.⇒.)
	) where

-- keyword.import.haskell, entity.name.module.haskell
import Control.Monad (Monad(..))
import Data.List
import Prelude
import Prelude.Unicode
import Data.Monoid (
	Monoid(
		mempty,
		-- foo,
		mappend),
	mconcat)
import Language.Haskell.TH
import qualified Data.Map as M
	(Map)

-- keyword.declaration.data.haskell, entity.name.data.haskell, entity.name.constructor.haskell, entity.name.class.haskell, variable.generic.haskell
data Dat a b = LeftDat a | RightDat b deriving (Eq, Ord, Read, Show)

data LinedData a b =
	-- Foo
	LeftLinedData a |
	RightLinedData b
	deriving (
		Eq,
		-- comment
		Ord, Read, Show)

data Record a = Record {
	record, record_ :: a,
	-- comment
	recordName :: String } deriving (Eq, Show)

data RecordOneLine a = RecordOneLine { recOne, recTwo :: String, (****) :: Int → Int → Int }

data GData a b where
	GLeft :: a -> GData a b
	-- comment
	GRight ∷ b → GData a b
	deriving (Show)

type Foo a b = Dat a b

-- keyword.declaration.instance.haskell
instance Functor (Dat a) where
	fmap _ (LeftDat x) = LeftDat x
	fmap f (RightDat x) = RightDat (f x)

-- entity.name.function.haskell, entity.name.class.haskell, entity.name.data.haskell, keyword.operator.haskell
fun :: Monoid a => [Dat a a] → a
fun = mconcat ∘ map (\case
	LeftDat x → x
	RightDat x → x)

fun2 :: (Num a, Show a) ⇒ [Dat a a] → String
fun2 = show ∘ sum ∘ map (\case
		LeftDat x → x
		RightDat x → negate x)

-- string.quoted.double.haskell
fun3 :: (Show a) ⇒ [Dat a a] → String
fun3 = intercalate ", " ∘ map show ∘ map (\case
	LeftDat x → x
	RightDat x → x)

fun4 :: (Show a) ⇒
	-- foo
	[Dat a a] →
	String
fun4 = fun3 where
	(.++.) :: Int -> Int -> Int
	(.++.) x y = x + y
	infixr 5 .++.

data SumDat a b = SumDat a b

plus :: a -> b -> SumDat a b
x `plus` y = SumDat x y

plus' ∷ a → b → SumDat a b
plus' = SumDat

minus :: Maybe Int → Maybe Int → Maybe Int
minus f@(Just n) (Just m)
	| n ≤ m = Just $ m - n
	| otherwise = Just $ n - m
Just n `minus` Nothing = Just n
Nothing `minus` Just m
	| m ≤ 0 = Just m
	| m ≡ 0 = Nothing
	| otherwise = Just (negate m)

doTest :: Int → IO ()
doTest 0 = return ()
doTest n = do
	print n
	doTest (n - 1)

z1, z2, z3 :: Int
z1 = 10
z2 = 10
Just z3 = Just 10

-- keyword.operator.infix-call.haskell, entity.name.function.haskell
(.+.) :: a → b → SumDat a b
x .+. y = x `plus` y

(.⇒.) ∷ a → b → [(a, b)]
x .⇒. y = [(x, y)]

-- keyword.declaration.class.haskell, entity.name.class.haskell
class
	ClassTest a
		where
			classTest :: a

-- keyword.declaration.instance.haskell, entity.name.class.haskell
instance
	(Monoid a, Monoid b) ⇒
	ClassTest (SumDat a b)
		where
			classTest = SumDat mempty mempty

-- punctuation.quasi-quoter.haskell, entity.name.function.haskell
testE :: ExpQ
testE = [| 1 + 2 |]

stringlit = "fooo"
charlit = 'x'
numlit = 123
listlit = [1, 2, 3]
floatlit = 1.2

-- punctuation.pragma.preprocessor.haskell, pragma.preprocessor.haskell
#if 0
-- punctuation.comment.haskell, comment.line.haskell, comment.block.haskell
-- not used
{- not used -}
not_used = undefined
not_used2 = error "foo"
#endif

-- keyword.declaration.foreign.haskell
foreign export ccall
	export_fun ::
	Int → IO ()
export_fun = print

foreign export ccall export_fun_2 :: Int → IO ()
export_fun_2 = print
