module Squares (sumOfSquares, squareOfSums, difference) where

sumOfSquares :: (Integral a) => a -> a
sumOfSquares n = sum $ map (^2) [1..n]

squareOfSums :: (Integral a) => a -> a
squareOfSums n = (^2) $ sum [1..n]

difference :: (Integral a) => a -> a
difference n = squareOfSums n - sumOfSquares n
