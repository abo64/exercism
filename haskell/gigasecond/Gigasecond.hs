module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime gigasecond
  where gigasecond = 1e9
