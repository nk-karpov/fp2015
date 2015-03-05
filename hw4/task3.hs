fibonacci :: Integer -> Integer

fibonacci n = if n >= 0 then rec 0 1 ((+)) n else rec 0 1 ((-)) (-n)
rec a b op n = if n == 0 then a else rec b (op a b) op (n - 1)
