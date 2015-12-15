module CryptoSquare (normalizePlaintext, squareSize, plaintextSegments,
                     ciphertext, normalizeCiphertext) where

import Data.Char
import Data.List.Split (chunksOf)

type PlainText = String
type NormalizedPlainText = String
type SquareSize = Int
type TextSegments = [String]
type NormalizedCipherText = String
type CipherText = String

normalizePlaintext :: PlainText -> NormalizedPlainText
normalizePlaintext =
  map toLower . filter isAlphaNum

squareSize :: PlainText -> SquareSize
squareSize =
 ceiling . sqrt . (fromIntegral :: Int -> Double) . length . normalizePlaintext

plaintextSegments :: PlainText -> TextSegments
plaintextSegments plainText =
  chunksOf (squareSize plainText) normalizedPlainText
  where normalizedPlainText = normalizePlaintext plainText

ciphertext :: PlainText -> CipherText
ciphertext = concat . cipherTextSegments

normalizeCiphertext :: PlainText -> NormalizedCipherText
normalizeCiphertext = unwords . cipherTextSegments

cipherTextSegments :: PlainText -> TextSegments
cipherTextSegments plainText =
  map foldSegments squareColumns
  where
    textSegments = plaintextSegments plainText
    squareColumns = [0..pred $ squareSize plainText]

    foldSegments :: Int -> CipherText
    foldSegments col = foldl (addRowChar col) "" textSegments

    addRowChar :: Int -> CipherText -> PlainText -> CipherText
    addRowChar col cipherText plainTextRow
      | col < length plainTextRow = cipherText ++ [plainTextRow !! col]
      | otherwise = cipherText
