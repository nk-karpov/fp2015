import Data.Functor
import Control.Applicative

data Triple a = Tr a a a deriving (Eq,Show)
instance Functor Triple where
	fmap g (Tr a b c) = Tr (g a) (g b) (g c)

instance Applicative Triple where
	pure a = Tr a a a
	(Tr f g h) <*> (Tr a b c) = Tr (f a) (g b) (h c)	
