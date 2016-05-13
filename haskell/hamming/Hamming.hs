module Hamming (distance) where

import Data.Composition ( (.:) )

distance :: String -> String -> Int
distance =
  sum .: zipWith nucleotideDistance
  where
    nucleotideDistance = fromEnum .: (/=)
