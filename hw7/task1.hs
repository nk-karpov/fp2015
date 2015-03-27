import Data.List (unfoldr)
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun (s, t) = if t < s then Nothing else Just(t, (s, pred t))
