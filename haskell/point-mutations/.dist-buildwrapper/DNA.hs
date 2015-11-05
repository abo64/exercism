module DNA (hammingDistance) where

hammingDistance :: String -> String -> Int
hammingDistance strand1 strand2 =
  length differentNucleotides
    where
      differentNucleotide (n1, n2) = n1 /= n2
      strands = (zip strand1 strand2)
      differentNucleotides = filter differentNucleotide strands
