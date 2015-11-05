module ETL (transform) where

import Data.Map (Map, empty, foldrWithKey, union, fromList)
import Data.Char (toLower)

type Letter = String
type Point = Int
type Letters = [Letter]
type NewScore = (Letter,Point)
type OldScoreFormat = Map Point Letters
type NewScoreFormat = Map Letter Point

transform :: OldScoreFormat -> NewScoreFormat
transform =
  foldrWithKey f empty
  where
    f :: Point -> Letters -> NewScoreFormat -> NewScoreFormat
    f point letters = union $ oldScoreToNewScoreFormat point letters

    oldScoreToNewScoreFormat :: Point -> Letters -> NewScoreFormat
    oldScoreToNewScoreFormat point letters =
      fromList (map (toNewScore point) letters)

    toNewScore :: Point -> Letter -> NewScore
    toNewScore point letter =
      (map toLower letter, point)
