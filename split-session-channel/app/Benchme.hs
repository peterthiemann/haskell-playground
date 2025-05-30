module Benchme (bench, onerunLib, onerunPrim, onerunSplit) where

import Control.Concurrent (ThreadId, forkIO, forkFinally, yield,
                           MVar, putMVar, takeMVar, newEmptyMVar)
import Control.Monad (when)
import qualified Lib

repeats :: [Int]
repeats = [1 .. 1000]

bench :: IO () -> Int -> IO ()
bench onerun n =
  if n == 0
  then return ()
  else onerun >> bench onerun (n-1)

onerunLib :: IO ()
onerunLib = do
  (x, y) <- Lib.create
  _ <- forkIO $ do
    rys <- mapM (const (Lib.receive y)) repeats
    sum rys `seq` Lib.close y
    -- print (sum rys)
  mapM_ (\i -> Lib.send i x) repeats
  Lib.close x

onerunPrim :: IO ()
onerunPrim = do
  x <- Lib.createPrim
  _ <- forkIO $ do
    rys <- mapM (const (Lib.receivePrim x)) repeats
    sum rys `seq` return ()
  mapM_ (\i -> Lib.sendPrim i x) repeats

onerunSplit :: IO ()
onerunSplit = do
  (x, y) <- Lib.create
  _ <- forkIO $ do
    rys <- mapM (const (Lib.receive y)) repeats
    sum rys `seq` Lib.close y
    -- print (sum rys)
  mapM_ (\i -> do z <- Lib.split x
                  Lib.send i z
                  Lib.drop z
            ) repeats
  Lib.close x

