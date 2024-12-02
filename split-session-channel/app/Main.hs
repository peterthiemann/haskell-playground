module Main (main) where

import Control.Concurrent (forkIO, forkFinally, yield,
                           MVar, putMVar, takeMVar, newEmptyMVar)
import Control.Monad (when)
import qualified Lib


main :: IO ()
main = do
  flags <- mapM runner [1 .. 100]
  waitFor flags

waitFor :: [MVar ()] -> IO ()
waitFor [] = return ()
waitFor (m : ms) = do
  takeMVar m
  waitFor ms

runner :: Int -> IO (MVar ())
runner i = do
  flag <- newEmptyMVar
  _tid <- flip forkFinally (const $ putMVar flag ()) $ do
    (x, y) <- Lib.create
    _tid <- forkIO $ server i y
    client i x
  return flag

client :: Int -> Lib.Session Int -> IO ()
client i x = do
  when (i `mod` 2 == 0) yield
  Lib.send 42 x
  Lib.send 17 x
  r <- Lib.receive x
  putStrLn (show r)
  Lib.close x

server :: Int -> Lib.Session Int -> IO ()
server i y = do
  when (i `mod` 2 /= 0) yield
  b <- Lib.split y
  _tid <- forkIO $ do { when ((i `mod` 4) `div` 2  == 1) yield; _r42 <- Lib.receive b; Lib.drop b }
  when ((i `mod` 4) `div` 2 == 0) yield
  _r17 <- Lib.receive y
  Lib.send (1000 + i) y
  Lib.close y
