module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.Map (Map)
import Data.List.Split (wordsBy)
import Data.MultiSet (fromList, toMap)

wordCount :: String -> Map String Int
wordCount = toMap . fromList . map toLowerCase . wordsBy (not . isAlphaNum)
  where toLowerCase = map toLower
