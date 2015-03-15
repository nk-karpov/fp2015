repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem _ ([]) = []
repeatEveryElem a (x:xs) = (rep a x) ++ (repeatEveryElem a xs)
rep 0 _ = []
rep a x = [x] ++ rep (a - 1) x
