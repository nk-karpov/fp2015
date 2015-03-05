seqA :: Integer -> Integer
seqA n = rec 1 2 3 n

rec a0 a1 a2 n = if n == 0 then a0 else rec a1 a2 (a2 + a1 - 2 * a0) (n - 1) 
