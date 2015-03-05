sqrtNewton :: Double -> Double
rec :: Double -> Double -> Int -> Double
sqrtNewton a = rec a 1 100
rec a x n = if n == 0 then x else rec a (x -  ((x * x - a) / (2 * x))) (n - 1)
