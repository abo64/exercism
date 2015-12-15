module Sieve (primesUpTo) where

primesUpTo :: Int -> [Int]
primesUpTo n =
  foldl addIfPrime [] candidates
  where candidates = [2..n]

addIfPrime :: [Int] -> Int -> [Int]
addIfPrime primes candidate
  | candidateIsPrime = primes ++ [candidate]
  | otherwise = primes
  where
    candidateIsPrime = all notDivisibleBy relevantPrimes
    notDivisibleBy i = candidate `rem` i > 0
    relevantPrimes = takeWhile squareLE primes
    squareLE i = i*i <= candidate