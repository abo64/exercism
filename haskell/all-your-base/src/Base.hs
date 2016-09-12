module Base (rebase) where

import Control.Monad (mfilter, foldM)
import Data.List (unfoldr)
import Data.Tuple (swap)

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits = do
  inBase <- validBase inputBase
  outBase <- validBase outputBase
  dec <- toDecimal inBase inputDigits
  return $ fromDecimal outBase dec

validBase :: Integral a => a -> Maybe a
validBase = mfilter (> 1) . Just

validDigit :: Integral a => a -> a -> Maybe a
validDigit base = mfilter (\x -> x > -1 && x < base) . Just

toDecimal :: Integral a => a -> [a] -> Maybe a
toDecimal base = foldM nextInt 0
  where
    nextInt acc = fmap (acc*base +) . validDigit base
--toDecimal base = fmap sum . zipWithM validMult powers . reverse
--  where
--    validMult p = fmap (p*) . validDigit base
--    powers = map (base^) [0,1..]
----    powers = unfoldr (\b -> Just (base^b, succ b)) 0

fromDecimal :: Integral a => a -> a -> [a]
fromDecimal base = reverse . unfoldr nextDigit
  where
    nextDigit =
      fmap (swap . (`quotRem` base)) . mfilter (> 0) . Just
--    nextDigit dec =
--      | dec > 0 = Just (swap $ quotRem dec base)
--      | otherwise = Nothing
