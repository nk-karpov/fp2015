import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
	fmap g (Tr a b c) = Tr (g a) (g b) (g c)

instance Applicative Triple where
	pure a = Tr a a a
	(Tr f g h) <*> (Tr a b c) = Tr (f a) (g b) (h c)	

instance Foldable Triple where
	foldMap f (Tr a b c) = f a `mappend` f b `mappend` f c

instance Traversable Triple where
	traverse f (Tr a b c) = Tr <$> f a <*> f b <*> f c
