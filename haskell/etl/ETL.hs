module ETL (transform) where

import qualified Data.Map as M
import Data.Char as Char (toLower)

type Point = Int
type UpperCaseLetter = String
type LowerCaseLetter = String
type NewScore = (LowerCaseLetter, Point)
type OldScoreFormat = M.Map Point [UpperCaseLetter]
type NewScoreFormat = M.Map LowerCaseLetter Point

transform :: OldScoreFormat -> NewScoreFormat
transform = M.fromList . newScores
  where
    newScores :: OldScoreFormat -> [NewScore]
    newScores oldScoreFormat = do
      (point, upperCaseLetters) <- M.toList oldScoreFormat
      lowerCaseLetter <- map toLowerCase upperCaseLetters
      return (lowerCaseLetter, point)
  
    toLowerCase :: String -> String
    toLowerCase = map Char.toLower
