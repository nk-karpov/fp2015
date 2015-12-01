import Data.List

data Cell = E  -- empty, пустая клетка
          | X  -- крестик 
          | O  -- нолик
          deriving (Eq,Show)

type Row a = [a]
type Board = Row (Row Cell)

-- Начальная конфигурация для поля размера n
iniBoard :: Int -> Board
iniBoard n = let row = replicate n E in replicate n row

win :: Cell -> Board -> Bool
win E _   = False
win x brd = foldr (\a b -> or [a == x, b]) False (concat [[diag 0 brd, diag 0 (rev brd)], line brd, line (transpose brd)])

line :: Board -> [Cell]
line (x:xs) = (foldr merge (head x) x):(line xs)
line _ = []

rev :: Board -> Board
rev (x:xs) = (reverse x):(rev xs)
rev _ = []

diag :: Int -> Board -> Cell
diag v (x:[]) = x !! v
diag v (x:xs) = merge (x !! v) (diag (v + 1) (xs))
diag _ _ = E

merge :: Cell -> Cell -> Cell
merge x y = if x == y then x else E

nxt :: Cell -> Board -> [Board]
nxt x brd = do 	if or [win O brd, win X brd] then [] else [()];
								a <- [0..(length brd) - 1];
								b <- [0..(length brd) - 1];
								if test a b brd then [()] else [];
								return (change x a b brd)

test :: Int -> Int -> Board -> Bool
test a b (x:xs) = if a == 0 then x !! b == E else test (a - 1) b xs

change :: Cell -> Int -> Int -> Board -> Board
change x a b (e:es) = (if a == 0 then change' x b e else e):change x (a - 1) b es
change _ _ _ ([]) = []
change' x b (e:es) = (if b == 0 then x else e):change' x (b - 1) es
change' _ _ ([]) = []

