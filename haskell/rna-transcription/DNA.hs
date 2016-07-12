module DNA (toRNA) where

import qualified Data.Map as Map

toRnaNucleotide :: Map.Map Char Char
toRnaNucleotide = Map.fromList [ ('G','C'), ('C','G'), ('T','A'), ('A','U') ]

toRNA :: String -> Maybe String
toRNA = mapM (`Map.lookup` toRnaNucleotide)
