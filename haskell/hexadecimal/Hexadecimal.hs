module Hexadecimal (hexToInt) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>), (<*>))

type HexChar = Char
type Hex = String

hexToInt :: Hex -> Int
hexToInt hex
  | (not . isValidHex) hex = 0
  | otherwise = foldl toInt 0 hex
  where
    toInt :: Int -> HexChar -> Int
    toInt int hexChar = int*16 + asInt hexChar

    asInt :: HexChar -> Int
    asInt hexChar = fromJust $ elemIndex hexChar hexChars

    isValidHex :: Hex -> Bool
    isValidHex =
      (&&) <$> (not . null) <*> all (`elem` hexChars)
--      (not . null) hex && all (`elem` hexChars) hex

hexChars :: [HexChar]
hexChars = ['0'..'9'] ++ ['a'..'f']