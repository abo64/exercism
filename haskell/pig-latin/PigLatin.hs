module PigLatin (translate) where

import Text.Regex.Posix ((=~))

type ClearText = String
type PigLatin = String

translate :: ClearText -> PigLatin
translate = unwords . map wordToPigLatin . words

wordToPigLatin :: ClearText -> PigLatin
wordToPigLatin word = case consonantMatch of
  [consonant, rest] -> rest ++ consonant ++ "ay"
  _ -> word ++ "ay"
  where
    (_,_,_,consonantMatch) = word =~ "^(qu|squ|[^aeiou]+)(.*)$" :: (String,String,String,[String])
