module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)
import Text.Regex.Posix ((=~))

parse :: String -> (String,String,String)
parse str
  | null parts = invalidParts
  | otherwise = (head parts, head $ tail parts, head $ tail $ tail parts)
  where
    digits = filter isDigit str
    (_,_,_,parts) = digits =~ phoneNumberPattern :: (String,String,String,[String])
    phoneNumberPattern = "^1?([0-9]{3})([0-9]{3})([0-9]{4})$"
    invalidParts = ("000", "000", "0000")

number :: String -> String
number str =
  area ++ prefix ++ lineNumber
  where
    (area, prefix, lineNumber) = parse str

areaCode :: String -> String
areaCode str = first
  where (first,_,_) = parse str

prettyPrint :: String -> String
prettyPrint str =
  "(" ++ area ++ ") " ++ prefix ++ "-" ++ lineNumber
  where
    (area, prefix, lineNumber) = parse str
