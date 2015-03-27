newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
	showsPrec 0 (Matrix x) = if null x then showString "EMPTY" else showsPrec 1 (Matrix x)
	showsPrec 1 (Matrix x) = if null x then showString "" else (showList (head x) . showsPrec 2 (Matrix (tail x)))
	showsPrec 2 (Matrix x) = if null x then showString "" else ((showChar '\n') . showList (head x) . showsPrec 2 (Matrix (tail x)))

