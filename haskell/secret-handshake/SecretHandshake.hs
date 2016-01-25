{-# LANGUAGE FlexibleInstances #-}
module SecretHandshake (handshake) where

import Numeric (readInt)
import Data.Char (digitToInt)
import qualified Data.Map.Strict as Map
import Data.List (unfoldr)

type Event = String
type Events = [Event]

class Handshake a where
  toInt :: a -> Int

instance Handshake Int where
  toInt = id

instance Handshake String where
  toInt = parseInt . readsBinary
    where
      readsBinary = readInt 2 (`elem` "01") digitToInt
      parseInt [(n,"")] = n
      parseInt _ = 0

handshake :: (Handshake a) => a -> Events
handshake = finalize . events
  where
    finalize ("reverse":xs) = xs
    finalize xs = reverse xs

    events :: (Handshake a) => a -> Events
    events = unfoldr unfoldEvents . toInt

    unfoldEvents n = fmap nextEvent lookupEvent
      where
        lookupEvent = Map.lookupLE n intToEvent
        nextEvent (i, e) = (e, n - i)

intToEvent :: Map.Map Int Event
intToEvent = Map.fromList
  [(16, "reverse"),(8,"jump"),(4,"close your eyes"),(2,"double blink"),(1,"wink")]
