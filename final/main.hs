import Data.List
import System.Random
import Control.Monad

genRandList :: IO [String]
genRandList = replicateM 800 genWord

genWord :: IO String
genWord =  do
              l <- randomRIO(4, 9)
              replicateM l $ randomRIO('a', 'e')

writeInitialData :: IO ()
writeInitialData = do
                    xs <- genRandList
                    writeFile "data.txt" $ intercalate "\n" xs

processData :: [String] -> [String]
processData xs = sort $ filter unique xs

unique :: String -> Bool
unique (x:xs) =  (notElem x xs) && (unique xs)
unique _ = True


main :: IO()
main = do
        writeInitialData
        content <- readFile "data.txt"
        writeFile "result.txt" $ intercalate "\n" $ processData $ lines content
