module ListOps (length, reverse, map, filter, foldr, foldl', (++), concat) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ b [] = b
foldl' f b (x:xs) =
  let next = f b x
  in next `seq` foldl' f next xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

length :: [a] -> Int
length = loop 0
  where
    loop acc  [] = acc
    loop acc (_:xs) = loop (acc + 1) xs

reverse :: [a] -> [a]
reverse = loop []
  where
    loop acc [] = acc
    loop acc (x:xs) = loop (x:acc) xs

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x:xs) 
  | f x = x : filter f xs
  | otherwise = filter f xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs
