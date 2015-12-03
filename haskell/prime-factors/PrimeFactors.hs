module PrimeFactors (primeFactors) where

import Data.List (unfoldr)

type PrimeFactors = [Int]

primeFactors :: Int -> PrimeFactors
primeFactors n =
  unfoldr findNext (n, 2)
  where
    findNext :: (Int, Int) -> Maybe (Int, (Int,Int))
    findNext (dividend, divisor)
      | dividend == 1 = Nothing
      | divisor `goesCleanlyInto` dividend = Just (divisor, (dividend `quot` divisor, divisor))
      | otherwise = findNext (dividend, divisor + 1)

    goesCleanlyInto :: Int -> Int -> Bool
    goesCleanlyInto divisor dividend =
      dividend `mod` divisor == 0
