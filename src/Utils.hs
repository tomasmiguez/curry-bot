module Utils (today) where

import Data.Time (LocalTime(localDay), getZonedTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (ZonedTime(zonedTimeToLocalTime))

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime
