module Queens (boardString, canAttack) where

import Data.Maybe (catMaybes)
import Control.Monad (mfilter)
import Data.List (intersperse)
import Data.List.Split (chunksOf)

type Row = Int
type Col = Int
type Position = (Row, Col)
type Queen = Maybe Position
type Chessboard = String
type Square = Char

boardString :: Queen -> Queen -> Chessboard
boardString whiteQueen blackQueen =
  concat rows
  where
    rows = map addSeparators $ chunksOf 8 boardSquares
    boardSquares = fmap getSquare boardPositions
    boardPositions = [ (row,col) | row <- [0..7], col <- [0..7] ]
    addSeparators = flip (++) "\n" . intersperse ' '

    getSquare :: Position -> Square
    getSquare position =
      head $ catMaybes [queenSquare 'W' whiteQueen, queenSquare 'B' blackQueen, Just '_']
      where
        queenSquare :: Square -> Queen -> Maybe Square
        queenSquare square = fmap (const square) . mfilter (== position)

canAttack :: Position -> Position -> Bool
canAttack position1 position2 =
  onSameRow || onSameCol|| onSameDiagonal
  where
    onSameRow = deltaRow == 0
    onSameCol = deltaCol == 0
    onSameDiagonal = deltaRow == deltaCol
    deltaRow = delta fst
    deltaCol = delta snd
    delta f = abs (f position1 - f position2)
