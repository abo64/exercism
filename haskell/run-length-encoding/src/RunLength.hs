module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = []
decode xs
  | null digits = y : decode ys
  | otherwise   = replicate n y ++ decode ys
  where
    (digits, y:ys) = span isDigit xs  -- assumes syntactically correct input
    n = read digits :: Int

encode :: String -> String
encode = concatMap encodedGroup . group
  where
    encodedGroup x = count x ++ [head x]
    count x
      | length x > 1 = show $ length x
      | otherwise    = ""
