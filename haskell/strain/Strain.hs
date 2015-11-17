module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep p =
  foldr keep' []
  where
    keep' x xs = if p x then x:xs else xs

discard :: (a -> Bool) -> [a] -> [a]
discard = keep . (not . )
