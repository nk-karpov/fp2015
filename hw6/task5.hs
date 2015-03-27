comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb _ [] = []
comb n l = (ff (head l) (comb (n - 1) (tail l))) ++ (comb n (tail l))
ff x [] = []
ff x l = [[x] ++ (head l)] ++ (ff x (tail l))
