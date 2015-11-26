module Clock (fromHourMin, toString) where

import Text.Printf (printf)

type Hour = Integer
type Min = Integer

data Clock = Clock Hour Min
  deriving (Eq, Show)

instance Num Clock where
  (+) (Clock h1 m1) (Clock h2 m2) = fromHourMin (h1 + h2) (m1 + m2)
  fromInteger = fromHourMin 0
  negate (Clock h m) = Clock (pred hoursPerDay - h) (minutesPerHour - m)
  abs = undefined
  signum = undefined
  (*) = undefined

fromHourMin :: Hour -> Min -> Clock
fromHourMin h m = Clock h' m'
  where
    m' = m `mod` minutesPerHour
    h' = (h + m `div` minutesPerHour) `mod` hoursPerDay

toString :: Clock -> String
toString (Clock h m) = printf "%02d:%02d" h m

hoursPerDay :: Hour
hoursPerDay = 24

minutesPerHour :: Min
minutesPerHour = 60
