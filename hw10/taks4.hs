import Data.IORef

factorial :: Integer -> IO Integer
factorial n = do
  r <- newIORef 1
  i <- newIORef 1
  while i (<= n) ( do
    ival <- readIORef i
    modifyIORef' r (* ival)
    modifyIORef' i (+ 1)
   )
  readIORef r

while :: IORef a -> (a -> Bool) -> IO () -> IO ()
while ref p action = do undefined
