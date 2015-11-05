module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

sumOfMultiples :: Integral a => [a] -> a -> a
sumOfMultiples factors upperBound =
  sum multiples
    where
      multiples = [ i | i <- [2..upperBound-1],  isMultiple i]
      isMultiple x = any (isDivisibleBy x) factors
      isDivisibleBy x y = x `mod` y == 0

sumOfMultiplesDefault :: Integral a => a -> a
sumOfMultiplesDefault = sumOfMultiples [3,5]
