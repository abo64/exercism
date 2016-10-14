module SumOfMultiples (sumOfMultiples) where

  sumOfMultiples :: Integral a => [a] -> a -> a
  sumOfMultiples factors limit =
--    sum $ filter isMultiple [1..pred limit]
--    sum multiples
    sum `of'` multiples
    where
      multiples = [ i | i <- [1..pred limit],  isMultiple i]
      isMultiple x = any (isDivisibleBy x) factors
      isDivisibleBy x = (== 0) . (mod x)
      of' = ($)  -- silly humor
