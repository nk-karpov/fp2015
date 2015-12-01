import System.Random

avgdev'' :: Int -> Int -> Double
avgdev'' k n = avgdev k n k 0 (randomRs (0, 1) (mkStdGen 42))

avgdev :: Int -> Int -> Int -> Double  -> [Int] -> Double
avgdev k n 0 res xs  = res / fromIntegral k
avgdev k n k' res xs = avgdev k n (k' - 1) (res + dev (take n xs)) (drop n xs)
dev :: [Int] -> Double
dev x = abs (fromIntegral (length x) / 2 - fromIntegral (sum x))
              
