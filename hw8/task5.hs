import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)


instance Foldable Tree where
	foldMap f (Nil) = mempty
	foldMap f (Branch l e r) = foldMap f l `mappend` f e `mappend` foldMap f r
 
instance Traversable Tree where
	traverse f Nil = pure Nil
	traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r

instance Functor Tree where
	fmap g Nil = Nil
	fmap g (Branch l x r) = Branch (fmap g l) (g x) (fmap g r)

instance Applicative Tree where
	pure a = Branch (pure a) a (pure a)
	Nil <*> _ = Nil
	_ <*> Nil = Nil
	(Branch fl f fr) <*> (Branch l x r) = Branch (fl <*> l) (f x) (fr <*> r)
