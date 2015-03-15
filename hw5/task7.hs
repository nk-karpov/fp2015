digits :: Integer -> [Integer]
digits 0 = [0]
digits x = digits' x
digits' 0 = []
digits' x = if x < 0 then digits' (-x) else digits' (quot x 10) ++ [rem x 10]


containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes x = containsAll 1 (digits x)
containsAll 10 _ = True
containsAll x y = and [(contain x y) == 1, containsAll (x + 1) y]
contain x [] = 0
contain x (a:as) = (if x == a then 1 else 0) + (contain x as)
