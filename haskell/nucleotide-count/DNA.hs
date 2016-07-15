module DNA (count, nucleotideCounts) where

import Data.List as L
import Data.Map as M

count :: Char -> String -> Either String Int
count char dna =
  toNucleotide char >> foldDNA countNucleotide 0 dna
  where
    countNucleotide counter nucleotide =
      counter + fromEnum (nucleotide == char)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = foldDNA countNucleotides zeroCounts
  where
    countNucleotides = flip $ M.adjust succ
    zeroCounts = M.fromList (L.map zeroCount nucleotides)
    zeroCount nucl = (nucl, 0)

foldDNA :: (a -> Char -> a) -> a -> String -> Either String a
foldDNA f zero = L.foldl accumulate (Right zero)
  where
    accumulate a char = do
      acc <- a
      nucleotide <- toNucleotide char
      return $ f acc nucleotide

toNucleotide :: Char -> Either String Char
toNucleotide char =
  if char `elem` nucleotides
    then Right char
    else Left $ "invalid nucleotide '" ++ [char] ++ "'"

nucleotides :: String
nucleotides = "ACGT"