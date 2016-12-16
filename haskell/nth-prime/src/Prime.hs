module Prime (nth) where

import Control.Monad (guard)

nth :: Int -> Maybe Integer
nth n = do
  guard  $ n > 0
  return $ nth' n 3 [2]

-- recursive Sieve of Erathosthenes
nth' :: Int -> Integer -> [Integer] -> Integer
nth' n candidate primes
  | n == length primes = last primes
  | otherwise = nth' n nextCandidate nextPrimes
  where
    nextCandidate = succ candidate
    nextPrimes = if candidateIsPrime then primes ++ [candidate] else primes
    candidateIsPrime = all notDivisibleBy relevantPrimes
    notDivisibleBy i = candidate `mod` i > 0
    relevantPrimes = takeWhile squareLE primes
    squareLE i = i*i <= candidate
