reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' a l = l ++ [a] 
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' l a = a:l
ini'' = []
