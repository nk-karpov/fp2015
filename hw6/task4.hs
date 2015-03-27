rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = if and [n >= 0, test n xs] then (drop n xs) ++ (take n xs) 
							else (drop m xs) ++ (take m xs) 
								where m = rem ((rem n l) + l) l
									where l = length xs

test 0 xs = True
test _ [] = False
test n xs = test (n - 1) (tail xs)
