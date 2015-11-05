module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p x = filter p x

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)
