module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isSpace, isLetter)
import Data.Maybe (mapMaybe)

abbreviate :: String -> String
-- abbreviate = mapMaybe acronymChar . (zip =<< (' ' :)) -- by pointfree.io
abbreviate xs = mapMaybe acronymChar charPairs
  where
    charPairs = zip (' ':xs) xs
    acronymChar (x, y)
      | (not . isLetter) x && isLetter y = Just $ toUpper y
      | (not . isUpper) x  && isUpper y  = Just y
      | otherwise = Nothing
