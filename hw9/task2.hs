lookups :: (Eq a) => a -> [(a,b)] -> [b]
lookups x ys = do {(k, v) <- ys; if k == x then [v] else []}
