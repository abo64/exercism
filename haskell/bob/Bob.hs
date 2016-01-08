module Bob (responseFor) where

import Data.Char
import qualified Data.Text as T
import Text.Regex.Posix

responseFor :: String -> String
responseFor text
  | isShouting text = "Whoa, chill out!"
  | isQuestion text = "Sure."
  | isSilence text = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    isQuestion = matches "^.+\\?$"
    isSilence = null . trim
    isShouting = allLetters .&&. noLowerCase

    matches pattern text = text =~ pattern :: Bool
    trim = T.unpack . T.strip . T.pack
    allLetters = any isLetter
    noLowerCase = not . any isLower
    (.&&.) f g a = f a && g a
