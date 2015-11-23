module Say (inEnglish) where

import qualified Data.Map.Strict as Map

type Translation = String
type Separator = String

inEnglish :: (Integral a) => a -> Maybe Translation
inEnglish 0 = Just "zero"
inEnglish n
  | n < 0 || n >= 10^12 = Nothing
  | otherwise = Just $ translate n ""

translate :: (Integral a) => a -> Separator -> Translation
translate 0 _ = ""
translate n separator =
  translationHead ++ translationTail
  where
    translationHead = separator ++ factorTranslation ++ foundTranslation
    translationTail = translate newN newSeparator
    Just (found, foundTranslation) = Map.lookupLE n lexicon
    factor
      | found >= 100 = n `quot` found
      | otherwise = 0
    factorTranslation
      | factor > 0 = translate factor "" ++ " "
      | otherwise = ""
    newN
      | factor > 0 = n `rem` found
      | otherwise = n - found
    newSeparator
      | found < 20 = ""
      | found >= 20 && found <= 90 = "-"
      | otherwise = " "

lexicon :: (Integral a) => Map.Map a Translation
lexicon = Map.fromList
  [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"),
   (6, "six"), (7, "seven"), (8, "eight"), (9, "nine"), (10, "ten"),
   (11, "eleven"), (12, "twelve"), (13, "thirteen"), (14, "fourteen"),
   (15, "fifteen"), (16, "sixteen"), (17, "seventeen"), (18, "eighteen"),
   (19, "nineteen"), (20, "twenty"), (30, "thirty"), (40, "forty"),
   (50, "fifty"), (60, "sixty"), (70, "seventy"), (80, "eighty"), (90, "ninety"),
   (100, "hundred"), (1000, "thousand"), (10^6, "million"), (10^9, "billion")]