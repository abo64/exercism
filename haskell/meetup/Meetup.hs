module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time
import Data.Time.Calendar (Day)

type Year = Int
type Month = Int

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

data Schedule = Teenth | First | Second | Third | Fourth | Last

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay = undefined
