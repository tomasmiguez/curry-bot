{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CurryBot (updateSlackIds, updateEmployees, sendTodayBirthdayReminder) where

import Slack (allUsersWithEmail, channelByName, sendMsg, User(email, id))
import BotDb (peopleByBirthdayRange, peopleBirthdayToday, updateSlackIdByEmail, refreshEmployees, lastBirthdayReminderDay, saveEvent)
import Bamboo (allEmployees, Employee(email))
import Person (listPeople, Person)
import Utils (today)
import Config (slackConfig, SlackConfig(..))

import Data.Maybe (fromJust, isJust, isNothing, catMaybes)
import Data.Time.Calendar
import Data.List ( intercalate )

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
  | lastReminderDay `leqMaybe` t = return ()
  | weekend t = return ()
  | dayOfWeek t == Monday = do
    people <- peopleByBirthdayRange (addDays (-2) t) t
    thisWeekPeople <- peopleByBirthdayRange (addDays 1 t) (addDays 7 t)
    sendEvent "birthdayReminder" [birthdayReminderMsg people, upcomingBirthdayMsg thisWeekPeople]
  | otherwise   = do
    people <- peopleBirthdayToday
    sendEvent "birthdayReminder"  [birthdayReminderMsg people]

weekend :: Day -> Bool
weekend d = dow == Saturday || dow == Sunday
  where dow = dayOfWeek d

leqMaybe :: Ord a => Maybe a -> a -> Bool
leqMaybe Nothing _ = False
leqMaybe (Just x) y = x >= y

birthdayReminderMsg :: [Person] -> Maybe String
birthdayReminderMsg [] = Nothing
birthdayReminderMsg people = Just $ "Cumplieron años: \n" ++ listPeople people

upcomingBirthdayMsg :: [Person] -> Maybe String
upcomingBirthdayMsg [] = Nothing
upcomingBirthdayMsg people = Just $ "Esta semana van a cumplir años: \n" ++ listPeople people

sendEvent :: String -> [Maybe String] -> IO()
sendEvent name msgs = do
  channel <- channelByName . (.channelName) =<< slackConfig
  _ <- sendMsg (intercalate "\n\n" $ catMaybes msgs) channel
  _ <- saveEvent name
  return ()
