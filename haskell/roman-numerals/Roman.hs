module Roman (numerals) where

import qualified Data.Map.Strict as Map

type Numerals = String

numerals :: Int -> Numerals
numerals 0 = ""
numerals int =
  (numeral ++) $ numerals newInt
  where
    Just (value, numeral) = Map.lookupLE int numeralConversions
    newInt = int - value

numeralConversions :: Map.Map Int Numerals
numeralConversions = Map.fromList
 [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"),
  (100, "C"),  (90, "XC"),  (50, "L"),  (40, "XL"),
  (10, "X"),   (9, "IX"),   (5, "V"),   (4, "IV"),
  (1, "I")]
