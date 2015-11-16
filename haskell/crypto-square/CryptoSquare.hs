module CryptoSquare (normalizePlaintext, squareSize, plaintextSegments,
                     ciphertext, normalizeCiphertext) where

import Data.Char
import Data.List.Split (chunksOf)

type PlainText = String
type NormalizedPlainText = String
type SquareSize = Int
type PlainTextSegments = [String]
type NormalizedCipherText = String
type CipherText = String

normalizePlaintext :: PlainText -> NormalizedPlainText
normalizePlaintext =
  map toLower . filter isAlphaNum

squareSize :: PlainText -> SquareSize
squareSize =
 ceiling . sqrt . fromIntegral . length

plaintextSegments :: PlainText -> PlainTextSegments
plaintextSegments plainText =
  chunksOf (squareSize normalizedPlainText) normalizedPlainText
  where normalizedPlainText = normalizePlaintext plainText

ciphertext :: PlainText -> CipherText
ciphertext =
  filter (not . isSpace) . normalizeCiphertext

normalizeCiphertext :: PlainText -> NormalizedCipherText
normalizeCiphertext =
  unwords . zipN . plaintextSegments

zipN :: [[a]] -> [[a]]
zipN [] = []
zipN xs = heads xs : zipN (tails xs)
  where
    heads :: [[a]] -> [a]
    heads = concatMap $ take 1

    tails :: [[a]] -> [[a]]
    tails = filter (not . null) . map (drop 1)
