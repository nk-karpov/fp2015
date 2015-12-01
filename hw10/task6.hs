import System.Random
import Control.Monad
import Control.Monad.State

avgdev :: Int -> Int -> IO Double
avgdev k n = do
  xs <- replicateM k (avg n)
  return $ (foldl (+) 0 xs) / (fromIntegral k)

avg :: Int -> IO Double
avg n = do
  xs <- replicateM n (randomRIO (0, 1) :: IO Int)
  let sum = fromIntegral $ foldl (+) 0 xs
  return $ abs (fromIntegral n / 2 - sum)
