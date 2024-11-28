module Lib
    ( create, receive, send, split, drop
    ) where

import Control.Concurrent
import Data.IORef

data PrimitiveChan a =
  PrimitiveChan { payload :: MVar a, sync :: MVar () }

data ChannelState a
  = Active { now :: MVar (PrimitiveChan a), next :: MVar (PrimitiveChan a) }
  | Dropped
  | Closed

type Chan a = IORef (ChannelState a)

-- * API of FreeST style synchronous channels

createPrim :: IO (PrimitiveChan a)
createPrim = do
  payload <- newEmptyMVar
  sync <- newEmptyMVar
  return $ PrimitiveChan payload sync

receivePrim :: PrimitiveChan a -> IO a
receivePrim pc = do
  a <- takeMVar (payload pc)
  putMVar (sync pc) ()
  return a
  

sendPrim :: a -> PrimitiveChan a -> IO ()
sendPrim a c = do
  putMVar (payload c) a
  takeMVar (sync c)
  return ()

-- * API with channel splitting

unwrap :: Chan a -> String -> (MVar (PrimitiveChan a) -> MVar (PrimitiveChan a) -> IO b) -> IO b
unwrap c msg action = do
  cs <- readIORef c
  case cs of
    Active now next ->
      action now next
    Dropped ->
      error ("trying to " ++ msg ++ " on dropped channel")
    Closed ->
      error ("trying to " ++ msg ++ " on closed channel")

-- |Create a new channel in active state 
create :: IO (Chan a)
create = do
  pc <- createPrim
  now <- newMVar pc
  next <- newEmptyMVar
  newIORef (Active now next)

receive :: Chan a -> IO a
receive c = 
  unwrap c "receive" $ \ now next -> do
    pc <- readMVar now
    receivePrim pc

send :: a -> Chan a -> IO ()
send a c =
  unwrap c "send" $ \ now next -> do
    pc <- readMVar now
    sendPrim a pc

-- |Split a channel by returning its borrow.
--  Splitting suspends the argument channel until it is released by the borrow
split :: Chan a -> IO (Chan a)
split c =
  unwrap c "split" $ \ now next -> do
    link <- newEmptyMVar
    borrow <- newIORef (Active now link)
    writeIORef c (Active link next)
    return borrow

-- |Drop a channel. Usually applied to a borrow to return possession to the original owner.
drop :: Chan a -> IO ()
drop c =
  unwrap c "drop" $ \ now next -> do
    pc <- takeMVar now
    putMVar next pc
    writeIORef c Dropped

-- |Close a channel. Not strictly necessary, but may free some resources.
close :: Chan a -> IO ()
  unwrap c "close " $ \ now next -> do
    _ <- takeMVar now
    writeIORef c Closed
