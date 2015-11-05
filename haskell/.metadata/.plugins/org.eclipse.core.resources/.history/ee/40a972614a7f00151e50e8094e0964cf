module LinkedList (new, next, isNil, datum, toList, fromList, nil, reverseLinkedList) where

data LinkedList a = Empty | Cons a (LinkedList a) deriving (Show, Eq, Read, Ord)

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next (Cons _ t) = t
next Empty = error "next called on Empty"

nil :: LinkedList a
nil = Empty

isNil :: LinkedList a -> Bool
isNil Empty = True
isNil _ = False

datum :: LinkedList a -> a
datum (Cons h _) = h
datum Empty = error "datum called on Empty"

toList :: LinkedList a -> [a]
toList Empty = []
toList (Cons h t) = h : toList t

fromList :: [a] -> LinkedList a
fromList = foldr Cons Empty
-- HLint wanted me to do it w/ a foldr instead
--fromList [] = Empty
--fromList (h:t) = Cons h (fromList t)

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list =
  reverseLoop list Empty
  where
    reverseLoop Empty result = result
    reverseLoop (Cons h t) result = reverseLoop t (Cons h result)

