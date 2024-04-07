{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CurryBot (updateSlackIds, updateEmployees, sendTodayBirthdayReminder) where

import Slack (allUsersWithEmail, channelByName, sendMsg, User(email, id))
import BotDb (peopleByBirthdayRange, peopleBirthdayToday, updateSlackIdByEmail, refreshEmployees, lastBirthdayReminderDay, saveBirthdayReminderEvent)
import Bamboo (allEmployees, Employee(email))
import Person (identifier, Person)
import Utils (today)
import Config (slackConfig, SlackConfig(..))

import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time.Calendar
import Data.List (intercalate)

updateSlackIds :: IO ()
updateSlackIds = do
  users <- allUsersWithEmail
  mapM_ (\u -> updateSlackIdByEmail u.id $ fromJust u.email) users

updateEmployees :: IO ()
updateEmployees = do
  employees <- allEmployees
  print $ filter (isNothing . (.email)) employees
  refreshEmployees $ filter (isJust . (.email)) employees

sendTodayBirthdayReminder :: IO ()
sendTodayBirthdayReminder = do
  lastReminderDay <- lastBirthdayReminderDay
  t <- today
  sendTodayBirthdayReminderCheckDay lastReminderDay t

sendTodayBirthdayReminderCheckDay :: Maybe Day -> Day -> IO ()
sendTodayBirthdayReminderCheckDay lastReminderDay t
  | leqMaybe lastReminderDay t = return ()
  | weekend t = return ()
  | dayOfWeek t == Monday = do
    people <- peopleByBirthdayRange (addDays (-2) t) t
    thisWeekPeople <- peopleByBirthdayRange (addDays 1 t) (addDays 7 t)
    sendTodayBirthdayReminderTo thisWeekPeople "Esta semana van a cumplir años: \n"
    sendTodayBirthdayReminderTo people "Cumplieron años: \n"
  | otherwise   = do
    people <- peopleBirthdayToday
    sendTodayBirthdayReminderTo people "Cumplieron años: \n"

sendTodayBirthdayReminderTo :: [Person] -> String -> IO ()
sendTodayBirthdayReminderTo people header
  | null people = return ()
  | otherwise   = do
    channel <- channelByName . (.channelName) =<< slackConfig
    _ <- sendMsg message channel
    _ <- saveBirthdayReminderEvent
    return ()
    where
      body = intercalate "\n" $ map (\p -> "- " ++ identifier p ++ ".") people
      message = header ++ body

weekend :: Day -> Bool
weekend d = dow == Saturday || dow == Sunday
  where dow = dayOfWeek d

leqMaybe :: Ord a => Maybe a -> a -> Bool
leqMaybe Nothing _ = False
leqMaybe (Just x) y = x >= y
