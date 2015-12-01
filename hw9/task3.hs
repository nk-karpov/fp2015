factor2 :: Integer -> [(Integer, Integer)]
factor2 n = do {p <- [1..(b n)]; if (rem n p) == 0 then [(p, quot n p)] else []}
b = b' 1
b' p n = if (p + 1) * (p + 1) > n then p else b' (p + 1) n
