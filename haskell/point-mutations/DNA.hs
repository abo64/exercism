module DNA (hammingDistance) where

hammingDistance :: String -> String -> Int
hammingDistance strand1 strand2 =
  sum nucleotideDistances
    where
      nucleotideDistances = zipWith nucleotideDistance strand1 strand2
      nucleotideDistance = (fromEnum .) . (/=)
