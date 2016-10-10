module Hamming (distance) where

--import Data.List  (genericLength)
import Safe.Exact (zipWithExactMay)

distance :: (Eq a, Integral i) => [a] -> [a] -> Maybe i
distance xs ys = count id <$> zipWithExactMay (/=) xs ys

count :: (Eq a, Integral i) => (a -> Bool) -> [a] -> i
--count p = genericLength . filter p  -- ok, but zero fun?! ;-)
count p [] = 0
count p (x:xs) | p x = 1 + count p xs
count p (_:xs) = count p xs
