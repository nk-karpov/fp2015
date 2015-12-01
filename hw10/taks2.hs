import Control.Monad.Writer
minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL x xs = foldl f (writer(x, show x)) xs
f y x = writer(v - x, "(" ++ l ++ "-" ++ show x ++ ")") where (v, l) = runWriter $ y
