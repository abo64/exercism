module Triangle (TriangleType(..), triangleType) where

import Data.List (nub)

data TriangleType = Equilateral | Isosceles | Scalene | Illogical
  deriving (Eq, Show)

type Side = Int

triangleType :: Side -> Side -> Side -> TriangleType
triangleType s1 s2 s3 =
  if not $ isLogical s1 s2 s3 then Illogical
  else case numEqualSides of
    3 -> Equilateral
    2 -> Isosceles
    _ -> Scalene
  where numEqualSides = (4 -) $ length $ nub [s1, s2, s3]

isLogical :: Side -> Side -> Side -> Bool
isLogical s1 s2 s3 =
 allSidesPositive && triangleInequality
 where
   allSidesPositive = s1 > 0 && s2 > 0 && 3 > 0
   triangleInequality = s1 + s2 > s3 && s2 + s3 > s1 && s1 + s3 > s2