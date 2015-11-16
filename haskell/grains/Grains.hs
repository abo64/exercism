module Grains (square, total) where

type Grains = Integer
type ChessboardSquares = Int

square :: ChessboardSquares -> Grains
square =
  (2^) . pred

total :: Grains
total =
  sum $ map square allChessboardSquares
    where
      allChessboardSquares = [1 .. 64]
