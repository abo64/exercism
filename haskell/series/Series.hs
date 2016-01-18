module Series (slices) where

import Data.Char (digitToInt)

type DigitStr = String
type Digit = Int
type Digits = [Digit]
type DigitSeries = [Digits]

slices :: Int -> DigitStr -> DigitSeries
slices _ [] = []
slices size digitStr
  | length digitStr >= size = slice : slices size (tail digitStr)
  | otherwise = []
  where
    slice = map digitToInt digitSlice
    digitSlice = take size digitStr
