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
  channel <- channelByName "general"
  birthdayPeople <- peopleBirthdayToday
  _ <- sendMsg (message birthdayPeople) channel
  return ()
  where
    header = "Hoy cumplen anios: \n"
    body people = "- " ++ (identifier =<< people) ++ ".\n"
    message people = header ++ body people
