module Luhn (checkDigit, addends, checksum, isValid, create) where

import Data.Char (digitToInt)

type Number = Integer
type Digit = Integer
type Digits = [Digit]
type Checksum = Integer

checkDigit :: Number -> Digit
checkDigit = last . addends

addends :: Number -> Digits
addends number =
  map transform indices
  where
    digits :: Digits
    digits = map (fromIntegral . digitToInt) $ show number
    numDigits = length digits
    indices = [0..numDigits-1]
    doLuhnTransform
      | even numDigits = even
      | otherwise = odd

    transform :: Int -> Integer
    transform index
      | doLuhnTransform index = luhnTransform digit
      | otherwise = digit
      where digit = digits !! index

    luhnTransform :: Integer -> Integer
    luhnTransform digit
      | doubledDigit >= 10 = doubledDigit - 9
      | otherwise = doubledDigit
      where doubledDigit = 2*digit

checksum :: Number -> Checksum
checksum = (`rem` 10) . sum . addends

isValid :: Number -> Bool
isValid = (==0) . checksum

create :: Number -> Number
create number =
  head $ filter isValid [ number*10 + x | x <- [0..9] ]
