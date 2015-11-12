module Raindrops (convert) where

type Raindrops = String

raindrops :: [(Int, Raindrops)]
raindrops =
  [(3, "Pling"), (5, "Plang"), (7, "Plong")]

convert :: Int -> Raindrops
convert number
  | null result = show number
  | otherwise = result
  where
    result = foldr addIfPrimeFactor "" raindrops

    addIfPrimeFactor (n, raindrop) acc =
      if hasPrimeFactor n then raindrop ++ acc else acc

    hasPrimeFactor :: Int -> Bool
    hasPrimeFactor = (==0) . mod number
