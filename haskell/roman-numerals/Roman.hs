module Roman (numerals) where

import Data.List (unfoldr, find)
import Control.Applicative ((<$>))

type Numerals = String
type NumeralConversion = (Numerals,Int)

numerals :: Int -> Numerals
numerals =
  concat <$> unfoldr nextNumeral

nextNumeral :: Int -> Maybe NumeralConversion
nextNumeral int =
  subtractFound <$> find lteInt numeralConversions
  where
    subtractFound (n,i) = (n, int - i)
    lteInt (_,i) = i <= int

numeralConversions :: [NumeralConversion]
numeralConversions =
 [("M", 1000), ("CM", 900), ("D", 500), ("CD", 400),
  ("C", 100),  ("XC", 90),  ("L", 50),  ("XL", 40),
  ("X", 10),   ("IX", 9),   ("V", 5),   ("IV", 4),
  ("I", 1)]