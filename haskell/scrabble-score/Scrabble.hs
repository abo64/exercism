module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

type Letter = Char
type Score = Int
type Word = [Letter]

scoreLetter :: Letter -> Score
scoreLetter letter
  | scores onePoint = 1
  | scores twoPoints = 2
  | scores threePoints = 3
  | scores fourPoints = 4
  | scores fivePoints = 5
  | scores eightPoints = 8
  | scores tenPoints = 10
  | otherwise = error ("unknown letter: " ++ [letter])
  where
    upperLetter = toUpper letter
    scores points = upperLetter `elem` points
    onePoint = ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T']
    twoPoints = ['D', 'G']
    threePoints = ['B', 'C', 'M', 'P']
    fourPoints = ['F', 'H', 'V', 'W', 'Y']
    fivePoints = ['K']
    eightPoints = ['J', 'X']
    tenPoints = ['Q', 'Z']

scoreWord :: Word -> Score
scoreWord = sum . map scoreLetter
