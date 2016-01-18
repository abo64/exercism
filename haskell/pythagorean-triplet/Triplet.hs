module Triplet (mkTriplet, isPythagorean, pythagoreanTriplets) where

import Data.List (permutations, sort)

type Triplet = (Int, Int, Int)
type PythagoreanTriplets = [Triplet]

mkTriplet :: Int -> Int -> Int -> Triplet
mkTriplet = (,,)

isPythagorean :: Triplet -> Bool
isPythagorean = any isPythagorean' . permutations . toList
  where
    toList (x, y, z) = [x, y, z]
    isPythagorean' [a, b, c] = a^2 + b^2 == c^2

pythagoreanTriplets :: Int -> Int -> PythagoreanTriplets
pythagoreanTriplets from to =
  sort [ (a,b,c) | c <- [from..to], b <- [from..c], a <- [from..b], a^2 + b^2 == c^2]
