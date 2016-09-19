module Base (rebase) where

import Control.Monad (foldM, guard)
import Data.Sequence (unfoldl)
import Data.Foldable (toList)

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits = do
  guard $ all (>= 2) [inputBase, outputBase]
  dec <- toNumber inputBase inputDigits
  return $ fromNumber outputBase dec

toNumber :: Integral a => a -> [a] -> Maybe a
toNumber base = foldM nextInt 0
  where
    nextInt acc digit = do
      guard $ digit >= 0 && digit < base
      return $ acc*base + digit

fromNumber :: Integral a => a -> a -> [a]
fromNumber base = toList . unfoldl nextDigit
  where
    nextDigit number = do
      guard $ number > 0
      return $ quotRem number base
--    nextDigit dec =
--      | dec > 0 = Just (quotRem dec base)
--      | otherwise = Nothing
