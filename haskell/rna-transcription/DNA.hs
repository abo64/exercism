module DNA (toRNA) where

import qualified Data.Map as Map
import Data.Traversable

toRnaNucleotide :: Map.Map Char Char
toRnaNucleotide = Map.fromList [ ('G','C'), ('C','G'), ('T','A'), ('A','U') ]

toRNA :: (Traversable t) => t Char -> Maybe (t Char)
toRNA = traverse (`Map.lookup` toRnaNucleotide)
