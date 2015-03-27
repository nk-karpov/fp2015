infixl 9  !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n 
fun e f n = if n < 0 then Nothing else if n == 0 then (Just e) else f (n - 1)
ini n = Nothing
