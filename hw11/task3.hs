import Control.Monad.Error

data ParseError = ParseError { location :: Int, reason :: String }

type ParseMonad = Either ParseError

table = "0123456789ABCDEF"
conv :: Char -> Int
conv 

parseHex :: String -> ParseMonad Integer
parseHex = undefined

printError :: ParseError -> ParseMonad String        
printError = 

-- тестирование
test s = str where
  (Right str) = do 
      n <- parseHex s
      return $ show n  
    `catchError` printError
