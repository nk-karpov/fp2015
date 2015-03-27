import Data.List (tails, inits)
tails' :: [a] -> [[a]]
tails' = foldr fun ini
fun a (x:xs) = (a:x):x:xs
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'
fun' a xs = ([]:(add a xs))
add a (x:xs) = (a:x):(add a xs)
add a ([]) = []
ini' = [[]]
