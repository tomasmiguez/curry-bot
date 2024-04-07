{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CurryBot (updateSlackIds, updateEmployees, sendTodayBirthdayReminder) where

import Slack
import BotDb
import Bamboo
import Person

import Data.Maybe (fromJust)
import Data.Time.Calendar
import Config (slackConfig, SlackConfig(..))
import Data.List (intercalate)

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
  lastReminderDay <- lastBirthdayReminderDay
  t <- today
  sendTodayBirthdayReminderCheckDay lastReminderDay t

sendTodayBirthdayReminderCheckDay :: Maybe Day -> Day -> IO ()
sendTodayBirthdayReminderCheckDay lastReminderDay t
  | maybeCompare lastReminderDay t = return ()
  | weekend t = return ()
  | dayOfWeek t == Monday = do
    people <- peopleByBirthdayRange (addDays (-2) t) t
    sendTodayBirthdayReminderTo people
  | otherwise   = do
    people <- peopleBirthdayToday
    sendTodayBirthdayReminderTo people

sendTodayBirthdayReminderTo :: [Person] -> IO ()
sendTodayBirthdayReminderTo people
  | null people = return ()
  | otherwise   = do
    channel <- channelByName . (.channelName) =<< slackConfig
    _ <- sendMsg message channel
    _ <- saveBirthdayReminderEvent
    return ()
    where
      header = "Cumplieron años: \n"
      body = intercalate "\n" $ map (\p -> "- " ++ identifier p ++ ".") people
      message = header ++ body

weekend :: Day -> Bool
weekend d = dow == Saturday || dow == Sunday
  where dow = dayOfWeek d

maybeCompare :: Ord a => Maybe a -> a -> Bool
maybeCompare Nothing _ = False
maybeCompare (Just x) y = x >= y
