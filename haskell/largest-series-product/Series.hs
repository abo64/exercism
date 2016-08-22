module Series (largestProduct) where

import Data.Char (digitToInt)
import Data.List (tails)

type Size = Int
type DigitString = String
type Digits = [Int]
type Slizes = [Digits]
type Product = Int

slices :: Size -> DigitString -> Slizes
slices size digitString =
  filter ((== size) . length) allSlices
  where
    allSlices = map (take size) $ tails digits
    digits = map digitToInt digitString

largestProduct :: Size -> DigitString -> Maybe Product
largestProduct s d
  | null theSlices = Nothing
  | otherwise = Just $ maximum $ map product theSlices
  where
    theSlices = slices s d
