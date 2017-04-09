module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram = flip all ['a'..'z'] . flip elem . map toLower
