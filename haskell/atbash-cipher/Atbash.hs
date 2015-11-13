module Atbash (encode) where

import Data.Char
import qualified Data.Map as M
import Data.List.Split

type Plain = String
type Cipher = String

encode :: Plain -> Cipher
encode plain =
   group ungroupedCipherText
  where
    ungroupedCipherText = atbashEncode $ filter isLetterOrDigit lowerPlain
    lowerPlain = map toLower plain
    isLetterOrDigit c = isAscii c && (isLetter c || isDigit c)

    atbashEncode = map atbashCipherChar
    atbashCipherChar c = M.findWithDefault c c atbashCipher

    group s = unwords $ chunksOf cipherGroupSize s
    cipherGroupSize = 5

atbashCipher :: M.Map Char Char
atbashCipher =
  M.fromList $ zip alphabet $ reverse alphabet
  where
   alphabet = ['a'..'z']