module Lib
    ( create, receive, send, split, Lib.drop, close, Session,
      createPrim, receivePrim, sendPrim
    ) where

import Control.Concurrent (MVar, readMVar, takeMVar, newEmptyMVar, newMVar, putMVar, modifyMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)


-- * Unsafe update operation for IORefs

modifyIORef :: IORef a -> (a -> IO (a, b)) -> IO b
modifyIORef r f = do
  a0 <- readIORef r
  (a1, b) <- f a0
  writeIORef r a1
  return b


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

modify :: Session a -> (SessionState a -> IO (SessionState a, b)) -> IO b
newSession :: MVar (PrimitiveChan a) -> MVar (PrimitiveChan a) -> IO (Session a)

-- type Session a = IORef (SessionState a)
-- modify = modifyIORef
-- newSession now next = newIORef $ Active now next

type Session a = MVar (SessionState a)
modify = modifyMVar
newSession now next = newMVar $ Active now next

unwrap :: Session a -> String -> (MVar (PrimitiveChan a) -> MVar (PrimitiveChan a) -> IO (SessionState a, b)) -> IO b
unwrap s msg action = do
  modify s go
    where
      -- go :: SessionState a -> IO (SessionState a, b)
      go (Active now next) = action now next                 
      go Dropped = error ("trying to " ++ msg ++ " on dropped channel")
      go Closed  = error ("trying to " ++ msg ++ " on closed channel")

-- | Create a new channel represented by two channel endpoints in active state 
create :: IO (Session a, Session a)
create = do
  pc <- createPrim
  now1 <- newMVar pc
  next1 <- newEmptyMVar
  chan1 <- newSession now1 next1
  now2 <- newMVar pc
  next2 <- newEmptyMVar
  chan2 <- newSession now2 next2
  return (chan1, chan2)

receive :: Session a -> IO a
receive c = 
  unwrap c "receive" $ \ now next -> do
    pc <- readMVar now
    rv <- receivePrim pc
    return (Active now next, rv)

send :: a -> Session a -> IO ()
send a c =
  unwrap c "send" $ \ now next -> do
    pc <- readMVar now
    sendPrim a pc
    return (Active now next, ())

-- | Split a channel by returning its borrow.
--  Splitting suspends the argument channel until it is released by the borrow
split :: Session a -> IO (Session a)
split c =
  unwrap c "split" $ \ now next -> do
    link <- newEmptyMVar
    borrow <- newSession now link
    return (Active link next, borrow)

-- | Drop a channel. Usually applied to a borrow to return possession to the original owner.
drop :: Session a -> IO ()
drop c =
  unwrap c "drop" $ \ now next -> do
    pc <- takeMVar now
    putMVar next pc
    return (Dropped, ())

-- | Close a channel. Not strictly necessary, but may free some resources.
close :: Session a -> IO ()
close c =
  unwrap c "close " $ \ now _next -> do
    _ <- takeMVar now
    return (Closed, ())
