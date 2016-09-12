module Base (rebase) where

import Control.Monad (mfilter, foldM)

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

fromDecimal :: Integral a => a -> a -> [a]
fromDecimal base = unfoldl nextDigit
  where
    nextDigit = fmap (`quotRem` base) . mfilter (> 0) . Just
--    nextDigit dec =
--      | dec > 0 = Just (quotRem dec base)
--      | otherwise = Nothing

unfoldl :: (b -> Maybe (b, a)) -> b -> [a]
unfoldl f seed = loop seed []
  where
    loop b as = maybe as (\(b,a) -> loop b (a:as)) (f b)
