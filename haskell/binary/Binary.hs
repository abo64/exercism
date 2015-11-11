module Binary (toDecimal) where

import Data.Maybe (fromMaybe)
import Control.Monad (mfilter)
import Data.Char (digitToInt)

type Decimal = Int
type Binary = String

toDecimal :: Binary -> Decimal
toDecimal binary =
  fromMaybe invalid (parseDecimal binary)
  where invalid = 0::Decimal

parseDecimal :: Binary -> Maybe Decimal
parseDecimal binary =
  fmap asDecimal validBinary
  where
    validBinary = mfilter isValidBinary (Just binary)
    isValidBinary :: Binary -> Bool
    isValidBinary = all isBinaryChar
    isBinaryChar :: Char -> Bool
    isBinaryChar char = char == '0' || char == '1'

    asDecimal :: Binary -> Decimal
    asDecimal = foldl increase 0
    increase acc x = acc * 2 + digitToInt x
