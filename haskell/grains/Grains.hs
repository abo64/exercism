module Grains (square, total) where

type Grains = Integer
type ChessboardSquares = Int

square :: ChessboardSquares -> Grains
square chessboardSquares =
  2 ^ (chessboardSquares - 1)

total :: Grains
total =
  sum $ map square allChessboardSquares
    where
      allChessboardSquares = [1 .. 64]
