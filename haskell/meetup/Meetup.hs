module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Time
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)

type Year = Integer
type Month = Int

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Eq)

data Schedule = Teenth | First | Second | Third | Fourth | Last

meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay schedule weekday year month = case schedule of
  Teenth -> next weekday $ fromGregorian year month 12
  First  -> next weekday $ previousDay $ fromGregorian year month 1
  Second -> nextWeek $ meetupDay First weekday year month
  Third  -> nextWeek $ meetupDay Second weekday year month
  Fourth -> nextWeek $ meetupDay Third weekday year month
  Last   -> previous weekday $ nextMonth $ fromGregorian year month 1

next :: Weekday -> Day -> Day
next = findWeekday 1

previous :: Weekday -> Day -> Day
previous = findWeekday (-1)

findWeekday :: Integer -> Weekday -> Day -> Day
findWeekday step weekday day =
  if found then candidate
  else findWeekday step weekday candidate
    where
      candidate = addDays step day
      (_, wd) = sundayStartWeek candidate
      candidatesWeekday = case wd of
        0 -> Sunday; 1 -> Monday; 2 -> Tuesday; 3 -> Wednesday
        4 -> Thursday; 5 -> Friday; 6 -> Saturday; _ -> undefined
      found = candidatesWeekday == weekday

previousDay :: Day -> Day
previousDay = addDays (-1)

nextWeek :: Day -> Day
nextWeek = addDays 7

nextMonth :: Day -> Day
nextMonth = addGregorianMonthsClip 1
