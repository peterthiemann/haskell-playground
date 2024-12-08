module Main (main) where

import Control.Concurrent (ThreadId, forkIO, forkFinally, yield,
                           MVar, putMVar, takeMVar, newEmptyMVar)
import Control.Monad (when)
import qualified Lib

type Sync = MVar ()

main :: IO ()
main = do
  flags <- fmap concat $ mapM runner [0 .. 999]
  waitFor flags

waitFor :: [Sync] -> IO ()
waitFor [] = return ()
waitFor (m : ms) = do
  takeMVar m
  waitFor ms

forkAndSync :: Sync -> IO a -> IO ThreadId
forkAndSync flag io =
  forkFinally io (const $ putMVar flag ())

----------------------------------------------------------------------

message :: String -> IO ()
message _ = return ()
-- message = putStrLn

----------------------------------------------------------------------

runner :: Int -> IO [Sync]
runner i = do
  flag <- newEmptyMVar
  flag_server <- newEmptyMVar
  flag_borrow <- newEmptyMVar
  _tid <- forkAndSync flag $ do
    (x, y) <- Lib.create
    _tid <- forkAndSync flag_server $ server flag_borrow i y
    client i x
  return [flag, flag_server, flag_borrow]

client :: Int -> Lib.Session Int -> IO ()
client i x = do
  when (i `mod` 2 == 0) yield
  Lib.send 42 x
  Lib.send 17 x
  r <- Lib.receive x
  putStrLn ("receive " ++ show r)
  message ("r-close " ++ show i)
  Lib.close x

server :: Sync -> Int -> Lib.Session Int -> IO ()
server flag i y = do
  when (i `mod` 2 /= 0) yield
  b <- Lib.split y
  _tid <- forkAndSync flag $ do
    when ((i `mod` 4) `div` 2  == 1) yield
    r42 <- Lib.receive b
    when (r42 /= 42) $ putStrLn ("wrong value in " ++ show (1000 + i))
    message ("b-drop " ++ show i)
    Lib.drop b
  when ((i `mod` 4) `div` 2 == 0) yield
  _r17 <- Lib.receive y
  Lib.send (1000 + i) y
  putStrLn ("s-close " ++ show (1000 + i))
  Lib.close y
