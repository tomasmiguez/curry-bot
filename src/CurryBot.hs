{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CurryBot (updateSlackIds, updateEmployees, sendTodayBirthdayReminder) where

import Slack
import BotDb
import Bamboo
import Person

import Data.Maybe (fromJust)
import Data.Time.Calendar

updateSlackIds :: IO ()
updateSlackIds = do
  users <- allUsersWithEmail
  mapM_ (\u -> updateSlackIdByEmail u.id $ fromJust u.email) users

updateEmployees :: IO ()
updateEmployees = do
  employees <- allEmployees
  upsertEmployees employees

sendTodayBirthdayReminder :: IO ()
sendTodayBirthdayReminder = do
  birthdayPeople <- peopleBirthdayToday
  lastReminderDay <- lastBirthdayReminderDay
  t <- today
  sendTodayBirthdayReminder' birthdayPeople lastReminderDay t

sendTodayBirthdayReminder' :: [Person] -> Maybe Day -> Day -> IO ()
sendTodayBirthdayReminder' people lastReminderDay t
  | null people || maybeCompare lastReminderDay t = return ()
  | otherwise   = do
    channel <- channelByName "general"
    _ <- sendMsg message channel
    _ <- saveBirthdayReminderEvent
    return ()
    where
      header = "Hoy cumplen años: \n"
      body = "- " ++ (identifier =<< people) ++ ".\n"
      message = header ++ body

maybeCompare :: Ord a => Maybe a -> a -> Bool
maybeCompare Nothing _ = False
maybeCompare (Just x) y = x >= y
