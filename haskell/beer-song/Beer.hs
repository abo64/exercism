module Beer (sing, verse) where

import qualified Data.Char as Char

type Verse = Int

verse :: Verse -> String
verse n =
  firstLine ++ secondLine
  where
    firstLine = capitalize $ bottlesOfBeerOnTheWall n ++ ", " ++ bottlesOfBeer n ++ endoOfLine
    secondLine =
      if n > 0 then "Take " ++ pronoun ++ " down and pass it around, " ++ bottlesOfBeerOnTheWall (n - 1) ++ endoOfLine
      else finalLine
    bottlesOfBeerOnTheWall x = bottlesOfBeer x ++ " on the wall"
    bottlesOfBeer x = bottles x ++ " of beer"
    bottles x
      | x == 0 = "no more bottles"
      | x == 1 = "1 bottle"
      | otherwise = show x ++ " bottles"
    capitalize (h:t) = Char.toUpper h : t
    endoOfLine = ".\n"
    pronoun = if n == 1 then "it" else "one"
    finalLine = "Go to the store and buy some more, " ++ bottlesOfBeerOnTheWall 99 ++ endoOfLine

sing :: Verse -> Verse -> String
sing from to =
  [ v | x <- [from,(from-1)..to], v <- verse x ++ "\n" ]
