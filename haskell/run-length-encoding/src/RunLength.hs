module RunLength (decode, encode) where

import Data.Char (isDigit, digitToInt)
import Data.List (group)

decode :: String -> String
decode = reverse . snd . foldl nextChar (0, "")
  where
    nextChar (n, xs) x
      | isDigit x = (n * 10 + digitToInt x, xs)
      | n == 0 = (0, x:xs)
      | otherwise = (0, replicate n x ++ xs)

encode :: String -> String
encode = concatMap encodedGroup . group
  where
    encodedGroup x = count x ++ [head x]
    count x = if length x > 1 then show $ length x else ""
