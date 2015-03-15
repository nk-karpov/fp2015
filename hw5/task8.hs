sublist :: Int -> Int -> [a] -> [a]
sublist _ _ ([]) = []
sublist a b (x:xs) = if b <= 0 then [] else (if a <= 0 then [x] else []) ++ sublist (a - 1) (b - 1) xs
