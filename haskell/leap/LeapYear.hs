module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year =
  let yearDivisibleBy = evenlyDivisibleBy year
  in yearDivisibleBy 4 &&
     (not (yearDivisibleBy 100) || yearDivisibleBy 400)
  where
    evenlyDivisibleBy x n = (x `mod` n) == 0

--evenlyDivisibleBy :: Int -> Int -> Bool
--evenlyDivisibleBy x n = (x `mod` n) == 0


