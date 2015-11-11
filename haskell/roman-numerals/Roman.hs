module Roman (numerals) where

import Data.List (unfoldr)
import Control.Applicative ((<$>))

type Numerals = String
type NumeralConversion = (Numerals,Int)
type NumeralConversions = [NumeralConversion]

numerals :: Int -> Numerals
numerals int =
  concat $ unfoldr nextNumeral int

nextNumeral :: Int -> Maybe NumeralConversion
nextNumeral int =
  maybeNext $ filter lteInt numeralConversions
  where
    maybeNext :: NumeralConversions -> Maybe NumeralConversion
    maybeNext ncs = diff <$> maybeHead ncs
    diff :: NumeralConversion -> NumeralConversion
    diff (n,i) =  (n, int - i)
    lteInt :: NumeralConversion -> Bool
    lteInt (_,i) = i <= int

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _ = Nothing

numeralConversions :: NumeralConversions
numeralConversions =
 [("M", 1000), ("CM", 900), ("D", 500), ("CD", 400),
  ("C", 100),  ("XC", 90),  ("L", 50),  ("XL", 40),
  ("X", 10),   ("IX", 9),   ("V", 5),   ("IV", 4),
  ("I", 1)]