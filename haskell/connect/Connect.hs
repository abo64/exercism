module Connect (resultFor, Color(Black, White)) where

import Data.List (union, (\\), nub)

data Color = Black | White
  deriving (Eq, Show)

type Field = Char
type Board = [[Field]]
type Row = Int
type Col = Int
type Coordinate = (Row, Col)

resultFor :: Board -> Maybe Color
resultFor board
  | wins White board = Just White
  | wins Black board = Just Black
  | otherwise = Nothing

wins :: Color -> Board -> Bool
wins color board =
  wins' (startCoordinates color board) []
  where
    wins' currentCoordinates visitedCoordinates
      | null currentCoordinates = False
      | any isGoal currentCoordinates = True
      | otherwise = wins' nextCurrent nextVisited
      where
        nextVisited = visitedCoordinates `union` currentCoordinates
        nextCurrent = nub (currentCoordinates >>= neighbors) \\ nextVisited

    isGoal = (== selector maxCoord) . selector
    selector = if color == Black then snd else fst
    maxCoord = maxCoordinate board

    neighbors :: Coordinate -> [Coordinate]
    neighbors (row, col) =
      filter (occupies color board)
        [(pred row, col), (pred row, succ col), (row, succ col),
         (succ row, col), (succ row, pred col), (row, pred col)]

maxCoordinate :: Board -> Coordinate
maxCoordinate board = (pred . length $ board, pred . length . head $ board)

occupies :: Color -> Board -> Coordinate -> Bool
occupies color board (row, col) =
  onBoard && (board !! row) !! col == field
  where
    onBoard = row >= 0 && row <= maxRow && col >= 0 && col <= maxCol
    (maxRow, maxCol) = maxCoordinate board
    field = if color == Black then 'X' else 'O'

startCoordinates :: Color -> Board -> [Coordinate]
startCoordinates color board =
  filter (occupies color board) candidateCoordinates
  where
    candidateCoordinates = map startCoord [0..selector maxCoord]
    startCoord = if color == Black then (\x -> (x,0)) else (\y -> (0,y))
    selector = if color == Black then snd else fst
    maxCoord = maxCoordinate board
