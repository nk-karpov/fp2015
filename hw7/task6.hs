import Prelude hiding (foldr, foldl, foldr1, foldl1)
import Data.Foldable
import Data.Monoid

data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

instance Foldable Tree where
	foldMap f (Nil) = mempty
	foldMap f (Branch l e r) = foldMap f l `mappend` f e `mappend` foldMap f r
instance Foldable Preorder where
	foldMap f (PreO (Nil)) = mempty
	foldMap f (PreO (Branch l e r)) = f e `mappend` foldMap f (PreO l) `mappend` foldMap f (PreO r)

instance Foldable Postorder where
	foldMap f (PostO (Nil)) = mempty
	foldMap f (PostO (Branch l e r)) = foldMap f (PostO l) `mappend` foldMap f (PostO r) `mappend` f e

instance Foldable Levelorder where
	foldMap f (LevelO t) = make f (lvl t) 

make f ([]) = mempty
make f (x:xs) = (make' f x) `mappend` (make f xs)
make' f ([]) = mempty
make' f (x:xs) = (f x) `mappend` (make' f xs)
lvl (Nil) = []
lvl (Branch l e r) = [e]:(z (lvl l) (lvl r))  
z ([]) ([]) = []
z (x:xs) ([]) = x:(z xs [])
z ([]) (x:xs) = x:(z xs [])
z (x:xs) (y:ys) = (x ++ y):(z xs ys)


