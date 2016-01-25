module Minesweeper (annotate) where

import Data.List.Split (chunksOf)
import Data.Char (intToDigit)

type RowString = String
type Board = [RowString]
type AnnotatedBoard = [RowString]

type Row = Int
type Col = Int
type Square = (Row, Col)
type Annotation = Char

annotate :: Board -> AnnotatedBoard
annotate board =
  map (map (annotateSquare board)) squares
  where
    squares = toSquares board

toSquares :: Board -> [[Square]]
toSquares board
  | null squares = []
  | otherwise = chunksOf cols squares
  where
    squares = [ (row, col) | row <- [0..pred rows], col <- [0..pred cols] ]
    (rows, cols) = boardDimensions board

annotateSquare :: Board -> Square -> Annotation
annotateSquare board square
  | isMine square = '*'
  | mineCount == 0 = squareContent square
  | otherwise = intToDigit mineCount
  where
    mineCount = length . filter isMine $ neighbors
    neighbors = adjacentSquares board square
    isMine sq = squareContent sq == '*'
    squareContent (x, y) = row !! y
      where row = board !! x

adjacentSquares :: Board -> Square -> [Square]
adjacentSquares board (r, c) =
  filter onBoard possibleNeigbors
  where
    possibleNeigbors = [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)]
    (rows, cols) = boardDimensions board
    onBoard (row, col) = row >= 0 && row < rows && col >= 0 && col < cols

boardDimensions :: Board -> (Row, Col)
boardDimensions board = (rows, cols)
  where 
    rows = length board
    cols = if null board then 0 else length $ head board
