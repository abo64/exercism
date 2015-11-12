module PrimeFactors (primeFactors) where

import Data.List (sort)

type PrimeFactors = [Int]

primeFactors :: Int -> PrimeFactors
primeFactors n =
  loop n 2 []
  where
    loop :: Int -> Int -> PrimeFactors -> PrimeFactors
    loop dividend divisor acc
      | dividend == 1 = sort acc
      | divisor `goesCleanlyInto` dividend = loop (dividend `div` divisor) divisor (divisor:acc)
      | otherwise = loop dividend (divisor + 1) acc

    goesCleanlyInto :: Int -> Int -> Bool
    goesCleanlyInto divisor dividend =
      dividend `mod` divisor == 0