{-# LANGUAGE TupleSections #-}
module DNA (count, nucleotideCounts) where

import Data.List as L
import Data.Map as M
import Control.Monad (foldM)

count :: Char -> String -> Either String Int
count char dna = do
  c <- toNucleotide char
  counts <- nucleotideCounts dna
  return $ counts ! c

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = foldM countNucleotides zeroCounts
  where
    zeroCounts = M.fromList $ L.map (, 0) nucleotides
    countNucleotides counts char = do
      nucleotide <- toNucleotide char
      return $ M.adjust succ nucleotide counts

toNucleotide :: Char -> Either String Char
toNucleotide char =
  if char `elem` nucleotides
    then Right char
    else Left $ "invalid nucleotide '" ++ [char] ++ "'"

nucleotides :: String
nucleotides = "ACGT"
