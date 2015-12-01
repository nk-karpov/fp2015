import Control.Monad.Writer
minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR x xs = foldr f (writer(x, show x)) xs
f x y = writer(x - v, "(" ++ show x ++ "-" ++ l ++ ")") where (v, l) = runWriter $ y
