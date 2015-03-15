movingLists :: Int -> [a] -> [[a]]
movingLists n (x:xs) = if test n (x:xs) then [slice n (x:xs)] ++ (movingLists n xs) else []
movingLists n ([]) = []
slice 0 _ = []
slice a (x:xs) = if test (a - 1) xs then [x] ++ slice (a - 1) xs else []
test 0 _ = True
test a ([]) = False
test a (x:xs) = test (a - 1) xs
