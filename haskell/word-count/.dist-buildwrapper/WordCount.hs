module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.Map (Map, empty, insertWith')
import Data.List.Split (wordsBy)

wordCount :: String -> Map String Int
wordCount text =
  foldl countWord empty theWords
  where
    theWords = wordsBy (not.isAlphaNum) lowerText
    countWord m w = insertWith' (+) w 1 m
    lowerText = map toLower text
