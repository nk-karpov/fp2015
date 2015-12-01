{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
import Control.Monad
import Control.Monad.Error
import Data.Char
-------------------1-------------------
data ListIndexError =
    ErrTooLargeIndex Int
    | ErrNegativeIndex
    | OtherErr String
    deriving (Eq, Show)
    
instance Error ListIndexError where
  noMsg = OtherErr "Unknown error"
  strMsg str = OtherErr str
  
atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast n (_:ys) = atLeast (n-1) ys
    
infixl 9 !!!
(!!!) :: (MonadError ListIndexError m) => [a] -> Int -> m a
xs !!! n
    | n < 0 = throwError ErrNegativeIndex
    | atLeast (n+1) xs = return (xs !! n)
    | otherwise = throwError $ ErrTooLargeIndex n
-------------------2---------------------
data Excep a = Err String | Ok a
    deriving (Eq, Show)

instance Monad Excep where
  return a = Ok a
  Ok a >>= f = f a
  Err m >>= _ = Err m
  fail s = Err "Monad.fail error."
  
instance MonadPlus Excep where
    mzero = Err "MonadPlus.mzero error."
    Err m `mplus` r = Err m
    l `mplus` r = r
    
instance (MonadError String) Excep where
    throwError = Err
    Err l `catchError` handler = handler l
    a `catchError` _ = a
    
-----------------2-test--------------
infixl 9 ?/
(?/):: (MonadError String m) => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x/y
--    
example :: Double -> Double -> Excep String
example x y = action `catchError` return where
    action = do
        q <- x ?/ y
        guard (q >=0)
        if q > 100 then do
            100 <- return q
            undefined
        else
            return $ show q
            
--GHCi> example 5 2
--Ok "2.5"
--GHCi> example 5 0
--Ok "Division by 0."
--GHCi> example 5 (-2)
--Ok "MonadPlus.mzero error."
--GHCi> example 5 0.002
--Ok "Monad.fail error."

---------------------3---------------------
data ParseError = ParseError {location::Int, reason::String}

type ParseMonad = Either ParseError

parseHex :: String -> ParseMonad Integer
parseHex = undefined

printError :: ParseError -> ParseMonad String
printError = undefined

------------------3-test--------------------
test s = str where
  (Right str) = do 
      n <- parseHex s
      return $ show n  
    `catchError` printError
    
--GHCi> test "DEADBEEF"
--"3735928559"
--GHCi> test "DEADMEAT"
--"At pos 5: M: invalid digit"
