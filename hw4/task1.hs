doubleFact :: Integer -> Integer
doubleFact n = if n > 1 then n * doubleFact (n - 2) else 1
