import Data.Functor
import Control.Applicative

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
	fmap g Nil = Nil
	fmap g (Branch l x r) = Branch (fmap g l) (g x) (fmap g r)

instance Applicative Tree where
	pure a = Branch (pure a) a (pure a)
	Nil <*> _ = Nil
	_ <*> Nil = Nil
	(Branch fl f fr) <*> (Branch l x r) = Branch (fl <*> l) (f x) (fr <*> r)
