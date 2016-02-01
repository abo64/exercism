module Frequency (frequency) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Char as C
import Control.Parallel.Strategies

type LetterFrequency = M.Map Char Int

frequency :: Int -> [T.Text] -> LetterFrequency
frequency howManyWorkers texts =
  addLetterFrequencies parLetterFrequencies
  where
    parLetterFrequencies = letterFrequencies `using` bufferedStrategy
    letterFrequencies = map letterFrequency texts
    bufferedStrategy = parBuffer howManyWorkers rseq

    addLetterFrequencies :: [LetterFrequency] -> LetterFrequency
    addLetterFrequencies = foldl addValues M.empty
      where addValues = M.unionWith (+)

letterFrequency :: T.Text -> LetterFrequency
letterFrequency = letterFrequency' . sanitizeText
  where
    letterFrequency' :: T.Text -> LetterFrequency
    letterFrequency' = T.foldl countLetter M.empty
    countLetter m c = M.insertWith (+) c 1 m

    sanitizeText = T.filter C.isLetter . T.map C.toLower
