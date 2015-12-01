{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Error
import Control.Monad

data ListIndexError = 
  ErrTooLargeIndex Int 
  | ErrNegativeIndex 
  | OtherErr String
  deriving (Eq, Show)

test :: Int -> [a] -> Bool
test _ [] = False
test 0 (x:xs) = True
test n (x:xs) = test (n - 1) xs

infixl 9 !!!
(!!!) :: (MonadError ListIndexError m) => [a] -> Int -> m a
xs !!! n
    | n < 0 = throwError ErrNegativeIndex
    | test n xs = return (xs !! n)
    | otherwise = throwError $ ErrTooLargeIndex n

