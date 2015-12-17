module Cipher (caesarEncode, caesarDecode, caesarEncodeRandom) where

import System.Random (newStdGen, randomR, randomRs)

type Key = String
type ClearText = String
type CipherText = String

caesarEncode :: Key -> ClearText -> CipherText
caesarEncode = shiftText id

caesarDecode :: Key -> CipherText -> ClearText
caesarDecode = shiftText negate

caesarEncodeRandom :: ClearText -> IO (Key, CipherText)
caesarEncodeRandom clearText = do
  key <- randomKey
  let cipherText = caesarEncode key clearText
  return (key, cipherText)

randomKey :: IO String
randomKey = do
  gen <- newStdGen
  let randomLetters = randomRs ('a', 'z') gen
      (size, _) = randomR (0, 100) gen
      key = take (100 + size) randomLetters
  return key

keyOffsets :: Key -> [Int]
keyOffsets key = map distanceFromA $ cycle key
  where distanceFromA char = fromEnum char - fromEnum 'a'

shiftText :: (Int -> Int) -> Key -> String -> String
shiftText direction key text =
  map (shiftChar direction) (zip text $ keyOffsets key)

shiftChar :: (Int -> Int) -> (Char, Int) -> Char
shiftChar direction (char, offset) = toEnum cyclicShifted
  where
    cyclicShifted
      | amount < 0 = upperBound + amount + 1
      | amount > maxOffset = lowerBound + amount - maxOffset - 1
      | otherwise = lowerBound + amount
    amount = fromEnum char - lowerBound + direction offset
    lowerBound = fromEnum 'a'
    upperBound = fromEnum 'z'
    maxOffset = length ['a'..'z'] - 1
