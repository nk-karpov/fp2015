digits :: Integer -> [Integer]
digits 0 = [0]
digits x = digits' x
digits' 0 = []
digits' x = if x < 0 then digits' (-x) else digits' (quot x 10) ++ [rem x 10]


