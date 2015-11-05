module Bob (responseFor) where

import Data.Char
import qualified Data.Text as T
import Text.Regex.Posix

responseFor :: String -> String
responseFor text
  | isShouting = "Whoa, chill out!"
  | matches "^.+\\?$" = "Sure."
  | trim == "" = "Fine. Be that way!"
  | otherwise = "Whatever."
    where
      matches pattern = text =~ pattern :: Bool
      trim = (T.unpack . T.strip . T.pack) text
      isShouting = any isLetter text && not (any isLower text)
