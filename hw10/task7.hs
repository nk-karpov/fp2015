import System.Random
import Control.Monad.State
import Control.Monad


randomRState :: (Random a, RandomGen g) => (a, a) -> State g a 
randomRState (x,y) = do 
  gen <- get
  let (r, gen') = randomR (x, y) gen
  put gen'
  return r

avgdev' :: Int -> Int -> State StdGen Double
avgdev' k n = do
  xs <- replicateM k (avg n)
  return $ sum xs / fromIntegral k

avg :: Int -> State StdGen Double
avg n = do
  xs <- replicateM n (randomRState (0, 1) :: State StdGen Int)
  return $ abs (fromIntegral (sum xs) - fromIntegral n / 2)
