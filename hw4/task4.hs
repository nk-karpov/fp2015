sum'n'count :: Integer -> (Integer, Integer)
absnk :: Integer -> Integer
sumnk :: Integer -> Integer
countnk :: Integer -> Integer
absnk x = if x < 0 then -x else x
sumnk x = if x == 0  then 0 else (rem x 10) + sumnk (quot x  10)
countnk x = if quot x  10 == 0 then 1 else (1 + countnk (quot x  10))
sum'n'count x = ((sumnk (absnk x)), (countnk (absnk x)))
