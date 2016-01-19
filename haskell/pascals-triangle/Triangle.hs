module Triangle (row, triangle) where

import Data.List (unfoldr)

row :: Integral a => Int -> [a]
row = last . flip take triangle

triangle :: Integral a => [[a]]
triangle =
  unfoldr addNextRow [1]
  where
    addNextRow row = Just (row, nextRow row)

    nextRow row = map (sumOfNeighbors row) [0..length row]

    sumOfNeighbors row position = left + right
      where
        left = if position == 0 then 0 else row !! pred position
        right = if position < length row then row !! position else 0