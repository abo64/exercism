module LeapYear (isLeapYear) where

isLeapYear :: Int -> Bool
isLeapYear year =
  yearDivisibleBy 4 &&
     (not . yearDivisibleBy) 100 || yearDivisibleBy 400
  where
    yearDivisibleBy = evenlyDivisibleBy year
    evenlyDivisibleBy x n = (x `mod` n) == 0
