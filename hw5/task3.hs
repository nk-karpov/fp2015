data Tree a = Leaf | Node (Tree a) a (Tree a)

treeSum :: Tree Integer -> Integer
treeSum (Leaf) = 0
treeSum (Node l a r) = a + (treeSum l) + (treeSum r)

treeHeight :: Tree a -> Int
treeHeight (Leaf) = 0
treeHeight (Node l a r) = 1 + max (treeHeight l) (treeHeight r)
