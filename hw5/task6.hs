digits :: Integer -> [Integer]
digits 0 = [0]
digits x = digits' x
digits' 0 = []
digits' x = if x < 0 then digits' (-x) else digits' (quot x 10) ++ [rem x 10]


containsAllDigits :: Integer -> Bool
containsAllDigits x = containsAll 1 (digits x)
containsAll 10 _ = True
containsAll x y = and [contain x y, containsAll (x + 1) y]
contain x [] = False
contain x (a:as) = or [(x == a), (contain x as)]

