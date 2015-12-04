module Octal (showOct, readOct) where

type Decimal = Int
type Octal = String

showOct :: (Integral a, Show a) => a -> Octal
showOct = decToOct ""
  where
    decToOct :: (Integral a, Show a) => Octal -> a -> Octal
    decToOct result dec
      | newDec == 0 = newResult
      | otherwise = decToOct newResult newDec
      where
        (newDec, remainder) = quotRem dec 8
        newResult = show remainder ++ result

readOct :: (Integral a) => Octal -> a
readOct oct
 | validOctal oct = fromIntegral $ octToDec 0 oct
 | otherwise = 0
  where
    octToDec :: Decimal -> Octal -> Decimal
    octToDec = foldl nextDec
      where
        nextDec result = (result*8 +) . digitToInt
-- only Prelude functions allowed ...
        digitToInt :: Char -> Int
        digitToInt c = read [c]::Int

    validOctal :: String -> Bool
    validOctal str = (not . null) str && all (`elem` "01234567") str
