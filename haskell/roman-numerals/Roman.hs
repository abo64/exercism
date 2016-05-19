module Roman (numerals) where

import qualified Data.Map.Strict as Map
import Data.List (unfoldr)
import Data.Functor ((<$>))
import Data.Tuple (swap)

type Numerals = String

numerals :: Int -> Numerals
numerals = concat . unfoldr go
  where
    go n = fmap (n-) . swap <$> Map.lookupLE n numeralConversions

numeralConversions :: Map.Map Int Numerals
numeralConversions = Map.fromList
 [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
  (100, "C"),  (90, "XC"),  (50, "L"),  (40, "XL"),
  (10, "X"),   (9, "IX"),   (5, "V"),   (4, "IV"),
  (1, "I")]
