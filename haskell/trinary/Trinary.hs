module Trinary (showTri, readTri) where

type Decimal = Int
type Trinary = String

base :: Int
base = 3

showTri :: (Integral a, Show a) => a -> Trinary
showTri = decToTri ""
  where
    decToTri :: (Integral a, Show a) => Trinary -> a -> Trinary
    decToTri result tri
      | newTri == 0 = newResult
      | otherwise = decToTri newResult newTri
      where
        (newTri, remainder) = quotRem tri $ fromIntegral base
        newResult = show remainder ++ result

readTri :: (Integral a) => Trinary -> a
readTri tri
 | validTrinary tri = fromIntegral $ triToDec 0 tri
 | otherwise = 0
  where
    triToDec :: Decimal -> Trinary -> Decimal
    triToDec = foldl nextDec
      where
        nextDec result = (result*base +) . digitToInt
-- only Prelude functions allowed ...
        digitToInt :: Char -> Int
        digitToInt c = read [c]::Int

    validTrinary :: String -> Bool
    validTrinary str = (not . null) str && all (`elem` trinaryDigits) str
      where
        trinaryDigits = concatMap show [0..base-1]
