module Deque (mkDeque, push, pop, shift, unshift) where

import Data.IORef

data LinkedList a = LinkedList { front::[a], back::[a] }
type Deque a = IORef (LinkedList a)

mkDeque :: IO (Deque a)
mkDeque = newIORef (LinkedList [] [])

modifyFront :: ([a] -> [a]) -> Deque a -> IO ()
modifyFront f deque =
  modifyIORef' deque (\(LinkedList fs bs) -> LinkedList (f fs) bs)

modifyBack :: ([a] -> [a]) -> Deque a -> IO ()
modifyBack f deque =
  modifyIORef' deque (\(LinkedList fs bs) -> LinkedList fs (f bs))

push :: Deque a -> a -> IO ()
push deque x = modifyBack (x:) deque

unshift :: Deque a -> a -> IO ()
unshift deque x = modifyFront (x:) deque

pop :: Deque a -> IO (Maybe a)
pop deque = do
  (LinkedList f b) <- readIORef deque
  if null b
  then do
    let x = last f
    modifyFront init deque
    return $ Just x
  else do
    let x = head b
    modifyBack tail deque
    return $ Just x

shift :: Deque a -> IO (Maybe a)
shift deque = do
  (LinkedList f b) <- readIORef deque
  if null f
  then do
    let x = last b
    modifyBack init deque
    return $ Just x
  else do
    let x = head f
    modifyFront tail deque
    return $ Just x
