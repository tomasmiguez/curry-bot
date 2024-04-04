{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CurryBot (updateSlackIds, updateEmployees, sendTodayBirthdayReminder) where

import Data.Maybe (fromJust)

import Slack
import BotDb
import Bamboo
import Person

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
  sendTodayBirthdayReminderTo birthdayPeople

sendTodayBirthdayReminderTo :: [Person] -> IO ()
sendTodayBirthdayReminderTo people
  | null people = return ()
  | otherwise   = do
    channel <- channelByName "general"
    _ <- sendMsg message channel
    return ()
    where
      header = "Hoy cumplen años: \n"
      body = "- " ++ (identifier =<< people) ++ ".\n"
      message = header ++ body
