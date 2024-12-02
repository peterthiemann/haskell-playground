module Lib
    ( create, receive, send, split, Lib.drop, close, Session
    ) where

import Control.Concurrent (MVar, readMVar, takeMVar, newEmptyMVar, newMVar, putMVar)
import Data.IORef

-- * API of FreeST style synchronous channels

data PrimitiveChan a =
  PrimitiveChan { payload :: MVar a, sync :: MVar () }

createPrim :: IO (PrimitiveChan a)
createPrim = do
  m_payload <- newEmptyMVar
  m_sync <- newEmptyMVar
  return $ PrimitiveChan m_payload m_sync
  
sendPrim :: a -> PrimitiveChan a -> IO ()
sendPrim a c = do
  putMVar (payload c) a
  takeMVar (sync c)

receivePrim :: PrimitiveChan a -> IO a
receivePrim pc = do
  a <- takeMVar (payload pc)
  putMVar (sync pc) ()
  return a

-- * API with channel splitting

data SessionState a
  = Active {-now-}  (MVar (PrimitiveChan a))
           {-next-} (MVar (PrimitiveChan a))
  | Dropped
  | Closed

type Session a = IORef (SessionState a)


unwrap :: Session a -> String -> (MVar (PrimitiveChan a) -> MVar (PrimitiveChan a) -> IO b) -> IO b
unwrap c msg action = do
  cs <- readIORef c
  case cs of
    Active now next ->
      action now next
    Dropped ->
      error ("trying to " ++ msg ++ " on dropped channel")
    Closed ->
      error ("trying to " ++ msg ++ " on closed channel")

-- | Create a new channel represented by two channel endpoints in active state 
create :: IO (Session a, Session a)
create = do
  pc <- createPrim
  now1 <- newMVar pc
  next1 <- newEmptyMVar
  chan1 <- newIORef (Active now1 next1)
  now2 <- newMVar pc
  next2 <- newEmptyMVar
  chan2 <- newIORef (Active now2 next2)
  return (chan1, chan2)

receive :: Session a -> IO a
receive c = 
  unwrap c "receive" $ \ now _next -> do
    pc <- readMVar now
    receivePrim pc

send :: a -> Session a -> IO ()
send a c =
  unwrap c "send" $ \ now _next -> do
    pc <- readMVar now
    sendPrim a pc

-- | Split a channel by returning its borrow.
--  Splitting suspends the argument channel until it is released by the borrow
split :: Session a -> IO (Session a)
split c =
  unwrap c "split" $ \ now next -> do
    link <- newEmptyMVar
    borrow <- newIORef (Active now link)
    writeIORef c (Active link next)
    return borrow

-- | Drop a channel. Usually applied to a borrow to return possession to the original owner.
drop :: Session a -> IO ()
drop c =
  unwrap c "drop" $ \ now next -> do
    pc <- takeMVar now
    putMVar next pc
    writeIORef c Dropped

-- | Close a channel. Not strictly necessary, but may free some resources.
close :: Session a -> IO ()
close c =
  unwrap c "close " $ \ now _next -> do
    _ <- takeMVar now
    writeIORef c Closed
