module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)
import Text.Regex.Posix ((=~))

type Parts = (String, String, String)

parse :: String -> Maybe Parts
parse str
  | null parts = Nothing
  | otherwise = Just (f, s, t)
  where
    digits = filter isDigit str
    (_,_,_,parts) = digits =~ phoneNumberPattern :: (String,String,String,[String])
    [f, s, t] = parts
    phoneNumberPattern = "^1?([0-9]{3})([0-9]{3})([0-9]{4})$"

parseAndThen :: (Parts -> a) -> String -> Maybe a
parseAndThen f = fmap f . parse

number :: String -> Maybe String
number = parseAndThen concatParts
  where concatParts (f, s, t) = f ++ s ++ t

areaCode :: String -> Maybe String
areaCode = parseAndThen first
  where first (f,_,_) = f

prettyPrint :: String -> Maybe String
prettyPrint = parseAndThen ppParts
  where ppParts (f, s, t) = "(" ++ f ++ ") " ++ s ++ "-" ++ t
