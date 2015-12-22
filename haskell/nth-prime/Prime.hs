module Prime (nth) where

nth :: Int -> Int
nth n = nth' n 3 [2]

-- recursive Sieve of Erathosthenes
nth' :: Int -> Int -> [Int] -> Int
nth' n candidate primes
  | length primes == n = last primes
  | otherwise = nth' n nextCandidate nextPrimes
  where
    nextCandidate = succ candidate
    nextPrimes = if candidateIsPrime then primes ++ [candidate] else primes
    candidateIsPrime = all notDivisibleBy relevantPrimes
    notDivisibleBy i = candidate `rem` i > 0
    relevantPrimes = takeWhile squareLE primes
    squareLE i = i*i <= candidate
