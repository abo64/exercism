module Palindromes (largestPalindrome, smallestPalindrome) where

import qualified Data.Map as M
--import Debug.Trace (trace)

type Palindrome a = (a,[(a, a)])

largestPalindrome :: (Show a, Integral a) => a -> a -> Palindrome a
largestPalindrome minFactor maxFactor =
  maximum $ allPalindromes minFactor maxFactor

-- | return value: (value, [(factor1, factor2)])
smallestPalindrome :: (Show a, Integral a) => a -> a -> Palindrome a
smallestPalindrome minFactor maxFactor =
  minimum $ allPalindromes minFactor maxFactor

allPalindromes :: (Show a, Integral a) => a -> a -> [Palindrome a]
allPalindromes minFactor maxFactor =
  mergeFactors valuesWithFactors
  where
    valuesWithFactors =
      [ (value, factors) | i <- [minFactor..maxFactor], j <- [minFactor..maxFactor],
                           let value = i * j, isPalindrome value, let factors = [(i, j)] ]
    mergeFactors = M.toList . M.fromListWith (++)
    isPalindrome x = decs == reverse decs
      where decs = show x
