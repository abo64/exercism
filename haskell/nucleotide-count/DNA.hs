module DNA (count, nucleotideCounts) where

import Data.List as L
import Data.Map as M

count :: Char -> String -> Int
count nucleotide dna
  | isNucleotide nucleotide && all isNucleotide dna = L.foldl accumulate 0 dna
  where
    accumulate acc nucl =
      if nucl == nucleotide then acc + 1 else acc

nucleotideCounts :: String -> Map Char Int
nucleotideCounts dna
  | all isNucleotide dna = M.mapWithKey countNucleotide zeroCounts
  where
    countNucleotide nucleotide _ = count nucleotide dna
    zeroCounts = M.fromList (L.map zeroCount nucleotides)
    zeroCount nucl = (nucl, 0)

isNucleotide :: Char -> Bool
isNucleotide char
  | char `elem` nucleotides = True
  | otherwise = error ("invalid nucleotide '" ++ [char] ++ "'")

nucleotides :: [Char]
nucleotides = ['A', 'C', 'G', 'T']