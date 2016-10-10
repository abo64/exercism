module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram

isAnagram :: String -> String -> Bool
isAnagram word candidate =
  lw /= lc && sort lw == sort lc
  where
    lw = toLowerCase word
    lc = toLowerCase candidate
    toLowerCase = map toLower
