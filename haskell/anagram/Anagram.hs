module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word candidates =
  filter sameCharsButNotIdentical candidates
    where
      lowerCaseWord = toLowerCase word
      wordChars = sortedChars lowerCaseWord
      sameChars str = sortedChars str == wordChars
      isIdentical str = toLowerCase str == lowerCaseWord
      sameCharsButNotIdentical candidate = sameChars candidate && (not . isIdentical) candidate
      toLowerCase str = map toLower str
      sortedChars str = sort (toLowerCase str)