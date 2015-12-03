module OCR (convert) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Data.List.Split (chunksOf)

type Grid = String
type Line = [String]
type PatternLine = [[String]]
type PatternLines = [PatternLine]
type RecognizedChars = String
type Pattern = [String]

convert :: Grid -> RecognizedChars
convert =
  intercalate "," . map recognizedCharsInLine . toPatternLines

recognizedCharsInLine :: PatternLine -> String
recognizedCharsInLine =
  map (flip (Map.findWithDefault '?') lexicon)

toPatternLines :: Grid -> PatternLines
toPatternLines =
  map lineToPatterns . chunksOf patternRows . lines
  where patternRows = 4

lineToPatterns :: Line -> [Pattern]
lineToPatterns line =
  map toPattern [0..patternsInLine-1]
  where
    patternsInLine = length top

    top = groupedPerPattern 0
    middleUp = groupedPerPattern 1
    middleDown = groupedPerPattern 2
    bottom = groupedPerPattern 3

    groupedPerPattern :: Int -> [String]
    groupedPerPattern = chunksOf patternCols . (line !!)
    patternCols = 3

    toPattern :: Int -> Pattern
    toPattern patternPos = map (!! patternPos) [top, middleUp, middleDown, bottom]

lexicon :: Map.Map Pattern Char
lexicon = Map.fromList [
  (zero, '0'), (one, '1'), (two, '2'), (three, '3'), (four, '4'),
  (five, '5'), (six, '6'), (seven, '7'), (eight, '8'), (nine, '9')]
  where
    zero =
     [ " _ "
     , "| |"
     , "|_|"
     , "   "]
    one =
     [ "   "
     , "  |"
     , "  |"
     , "   "]
    two =
     [ " _ "
     , " _|"
     , "|_ "
     , "   "]
    three =
     [ " _ "
     , " _|"
     , " _|"
     , "   "]
    four =
     [ "   "
     , "|_|"
     , "  |"
     , "   "]
    five =
     [ " _ "
     , "|_ "
     , " _|"
     , "   "]
    six =
     [ " _ "
     , "|_ "
     , "|_|"
     , "   "]
    seven =
     [ " _ "
     , "  |"
     , "  |"
     , "   "]
    eight =
     [ " _ "
     , "|_|"
     , "|_|"
     , "   "]
    nine =
     [ " _ "
     , "|_|"
     , " _|"
     , "   "]
