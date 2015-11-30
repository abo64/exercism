module Matrix ( Matrix, row, column, rows, cols, shape, Matrix.transpose, reshape, flatten, fromString, fromList) where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Arrow ((&&&))
import Data.List as L
import Data.List.Split (chunksOf)

type Row a = (Vector a)
type Column a = (Vector a)
type Dimension = Int
type Shape = (Dimension, Dimension)

data Matrix a = Matrix { rows::Dimension, cols::Dimension, vector::Vector a }
  deriving (Eq, Show)

row :: Int -> Matrix a -> Row a
row r = V.fromList . (!! r) . toList
--row r (Matrix _ c v) = V.slice (c * r) c v

-- easy way out: use List instead of Vector functions
column :: Int -> Matrix a -> Column a
column c = V.fromList . map (!! c) . toList

--rows :: Matrix a -> Dimension
--rows (Matrix r _ _) = r

--cols :: Matrix a -> Dimension
--rows (Matrix _ c _) = c

shape :: Matrix a -> Shape
shape = rows &&& cols

reshape :: Shape -> Matrix a -> Matrix a
reshape (r, c) (Matrix _ _ v) = Matrix r c v

transpose :: Matrix a -> Matrix a
transpose = fromList . L.transpose . toList

flatten :: Matrix a -> Vector a
flatten = vector

fromString :: Read a => String -> Matrix a
fromString = fromList . map readRec . lines
  where
    readRec s = case reads s of
      [(x, xs)] -> x : readRec xs
      _ -> []

fromList :: [[a]] -> Matrix a
fromList ls = Matrix r c v
  where
    r = length ls
    c = if null ls then 0 else length $ head ls
    v = V.fromList $ L.concat ls

toList :: Matrix a -> [[a]]
toList (Matrix _ c v) =
  chunksOf c $ V.toList v
