sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 x y z = sum2 (sum2 x y) z


sum2 :: Num a => [a] -> [a] -> [a]
sum2 ([]) y = y
sum2 x ([]) = x
sum2 (x:xs) (y:ys) = [x + y] ++ (sum2 xs ys)
