{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Error
import Control.Monad

data Excep a =  Err String | Ok a 
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
-- тестирование
(?/) :: (MonadError String m) => Double -> Double -> m Double
x ?/ 0 = throwError "Division by 0."
x ?/ y = return $ x / y

example :: Double -> Double -> Excep String
example x y = action  `catchError` return where 
  action = do 
    q <- x ?/ y
    guard (q >=0)
    if q  > 100 then do 
      100 <- return q
      undefined
    else 
      return $ show q
