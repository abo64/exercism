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
    isShouting = someLetter .&&. noLowerCase

    matches p = (=~ p)
    trim = T.unpack . T.strip . T.pack
    someLetter = any isLetter
    noLowerCase = not . any isLower
    (.&&.) f g a = f a && g a
