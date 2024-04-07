{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module BotDb (peopleByBirthdayRange, today, peopleBirthdayToday, updateSlackIdByEmail, refreshEmployees, lastBirthdayReminderDay, saveBirthdayReminderEvent) where

import Config (connStr)
import Person
import Bamboo (Employee(..))

import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.Calendar
import Data.Time.LocalTime
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)

conn :: IO Connection
conn = connStr >>= connectPostgreSQL

--- TODO: Take into account beginning and end on different years
--- ie from 30-12-2021 to 3-1-2022. For this cases we should insert another two dates in the middle, the last date
--- of the previous year and the first date of the new one, and make it in two ranges.
peopleByBirthdayRange :: Day -> Day -> IO [Person]
peopleByBirthdayRange beg end = do
  let
    (begYear, begMonth, begDay) = toGregorian beg
    (endYear, endMonth, endDay) = toGregorian end
    query = "SELECT \n\
            \  slack_id, email, first_name, last_name, date_of_birth \n\
            \FROM people \n\
            \WHERE \n\
            \  make_date(?, \n\
            \            EXTRACT(MONTH FROM date_of_birth)::int, \n\
            \            EXTRACT(DAY FROM date_of_birth)::int) \n\
            \  BETWEEN \n\
            \    make_date(?, ?, ?) \n\
            \    AND \n\
            \    make_date(?, ?, ?);"
  c <- conn
  r <- quickQuery' c query [toSql begYear, toSql begYear, toSql begMonth, toSql begDay, toSql endYear, toSql endMonth, toSql endDay]
  return $ map rowToPeople r
  where
    rowToPeople :: [SqlValue] -> Person
    rowToPeople [sqlSlackId, sqlEmail, sqlFirstName, sqlLastName, sqlDateOfBirth] =
      Person { slackId = fromSql sqlSlackId
              , email = fromSql sqlEmail
              , firstName = fromSql sqlFirstName
              , lastName = fromSql sqlLastName
              , dateOfBirth = fromSql sqlDateOfBirth }
    rowToPeople x = error $ "Unexpected result: " ++ show x

peopleBirthdayToday :: IO [Person]
-- peopleBirthdayToday = join $ peopleByBirthdayRange <$> today <*> today
peopleBirthdayToday = do
  t <- today
  peopleByBirthdayRange t t

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime

-- Handlear bien los dos casos, que pasa si no encuentra a nadie en la DB pero si en Slack?
updateSlackIdByEmail :: String -> String -> IO ()
updateSlackIdByEmail slackId email = do
  c <- conn
  r <- run c "UPDATE people SET slack_id = ? WHERE email = ?" [toSql slackId, toSql email]
  when (r /= 1) $ do
    putStrLn $ "Couldn't update person's slack ID: " ++ slackId ++ ", " ++ email
  when (r == 1) $ do
    putStrLn "Succesfully updated slack_id."
  commit c
  disconnect c

refreshEmployees :: [Employee] -> IO ()
refreshEmployees employees = do
  _ <- upsertEmployees employees
  _ <- deleteMissingEmployees employees
  return ()

upsertEmployees :: [Employee] -> IO ()
upsertEmployees employees = do
  let values = (\e -> [toSql e.email, toSql e.firstName, toSql e.lastName, toSql e.dateOfBirth]) =<< employees
      placeholders = intercalate "," (replicate (length employees) "(?,?,?,?)")
      updateStmt = "UPDATE SET first_name = EXCLUDED.first_name, last_name = EXCLUDED.last_name, date_of_birth = EXCLUDED.date_of_birth"
      query = "INSERT INTO people (email, first_name, last_name, date_of_birth) VALUES "
              ++ placeholders
              ++ " ON CONFLICT (email) DO "
              ++ updateStmt
  c <- conn
  _ <- run c query values
  commit c
  disconnect c
  return ()

deleteMissingEmployees :: [Employee] -> IO ()
deleteMissingEmployees employees = do
  let values = map (toSql . (.email)) employees
      placeholders = "(" ++ intercalate "," (replicate (length employees) "?") ++ ")"
      query = "DELETE FROM people WHERE email NOT IN " ++ placeholders
  c <- conn
  _ <- run c query values
  commit c
  disconnect c
  return ()

lastBirthdayReminderDay :: IO (Maybe Day)
lastBirthdayReminderDay = do
  c <- conn
  r <- quickQuery' c "SELECT ocurred_at FROM events WHERE type = 'birthdayReminder' ORDER BY ocurred_at DESC LIMIT 1" []
  disconnect c
  return $ localDay . zonedTimeToLocalTime . fromSql . head <$> listToMaybe r

saveBirthdayReminderEvent :: IO ()
saveBirthdayReminderEvent = do
  c <- conn
  _ <- run c "INSERT INTO events (type, ocurred_at) VALUES ('birthdayReminder', NOW())" []
  commit c
  disconnect c
  return ()
