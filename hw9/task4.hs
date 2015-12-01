absDiff :: Num a => [a] -> [a]
absDiff xs = do {(x, y) <- make xs; return (abs (x - y))}
make (x:y:xs) = (x, y):make (y:xs)
make _ = []

