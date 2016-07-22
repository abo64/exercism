module Hamming (distance) where

import Control.Applicative ((<$>), (<*>))

distance :: String -> String -> Maybe Int
distance [] [] = Just 0
distance (x:xs) (y:ys) =
  (+) <$> Just nucleotideDistance <*> distance xs ys
  where nucleotideDistance = fromEnum $ x /= y
distance _ _ = Nothing
