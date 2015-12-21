module CustomSet (CustomSet
  , empty
  , delete
  , difference
  , isDisjointFrom
  , null
  , intersection
  , member
  , insert
  , size
  , isSubsetOf
  , fromList
  , toList
  , union) where

import Prelude hiding (null)
import qualified Data.List as L

newtype CustomSet a = CustomSet { elements :: [a] }
  deriving (Eq)

instance (Show a) => Show (CustomSet a) where
  show = ("fromList " ++) . show . elements

empty :: CustomSet a
empty = CustomSet []

delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete x = CustomSet . L.delete x . elements
--delete x = CustomSet . filter (/= x) . elements

lift2 :: (Eq a) => ([a] -> [a] -> [a]) -> CustomSet a -> CustomSet a -> CustomSet a
lift2 f cs1 cs2 = CustomSet $ f (elements cs1) (elements cs2)

lift2' :: (Eq a, Ord a) => ([a] -> [a] -> [a]) -> CustomSet a -> CustomSet a -> CustomSet a
lift2' f cs1 cs2 = fromList $ f (elements cs1) (elements cs2)

difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference = lift2 (L.\\)
--difference (CustomSet xs) (CustomSet ys) = CustomSet $ xs L.\\ ys

isDisjointFrom :: (Eq a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom xs = (== empty) . intersection xs

null :: CustomSet a -> Bool
null (CustomSet []) = True
null _ = False

intersection :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
intersection = lift2 L.intersect
--intersection (CustomSet xs) (CustomSet ys) = CustomSet $ xs `L.intersect` ys

member :: (Eq a) => a -> CustomSet a -> Bool
member x = L.elem x . elements

insert :: (Ord a) => a -> CustomSet a -> CustomSet a
insert x = CustomSet . L.nub . L.insert x . elements
--insert x = fromList . (:) x . elements

size :: CustomSet a -> Int
size = L.length . elements

isSubsetOf :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf subSet superSet = superSet == union subSet superSet

fromList :: (Eq a, Ord a) => [a] -> CustomSet a
fromList = CustomSet . L.sort . L.nub

toList :: CustomSet a -> [a]
toList = elements

union :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union = lift2' L.union
--union (CustomSet xs) (CustomSet ys) = fromList $ xs `L.union` ys
