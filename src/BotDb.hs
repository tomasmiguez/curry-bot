{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module BotDb (peopleBirthdayToday, updateSlackIdByEmail, upsertEmployees) where

import Config (connStr)
import Person
import Bamboo (Employee(..))

import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.Calendar
import Data.Time.LocalTime
import Control.Monad (when)
import Data.List (intercalate)

conn :: IO Connection
conn = connStr >>= connectPostgreSQL

peopleByBirthday :: Day -> IO [Person]
peopleByBirthday d = do
  c <- conn
  r <- quickQuery' c "SELECT slack_id, email, first_name, last_name, birthday FROM people where birthday = ?" [toSql d]
  return $ map rowToPeople r
  where
    rowToPeople [sqlSlackId, sqlEmail, sqlFirstName, sqlLastName, sqlBirthday] =
      Person { slackId = fromSql sqlSlackId
             , email = fromSql sqlEmail
             , firstName = fromSql sqlFirstName
             , lastName = fromSql sqlLastName
             , birthday = fromSql sqlBirthday }
    rowToPeople x = error $ "Unexpected result: " ++ show x

peopleBirthdayToday :: IO [Person]
peopleBirthdayToday = peopleByBirthday . localDay . zonedTimeToLocalTime =<< getZonedTime

-- Handlear bien los dos casos, que pasa si no encuentra a nadie en la DB pero si en Slack?
updateSlackIdByEmail :: String -> String -> IO ()
updateSlackIdByEmail slackId email = do
  c <- conn
  r <- run c "UPDATE people SET slack_id = ? WHERE email = ?" [toSql slackId, toSql email]
  when (r /= 1) $ do
    putStrLn "Couldn't update person's slack ID."
  when (r == 1) $ do
    putStrLn "Succesfully updated slack_id."
  commit c

upsertEmployees :: [Employee] -> IO ()
upsertEmployees employees = do
  let values = (\e -> [toSql e.email, toSql e.firstName, toSql e.lastName, toSql e.birthday]) =<< employees
      placeholders = intercalate "," (replicate (length employees) "(?,?,?,?)")
      updateStmt = "UPDATE SET first_name = EXCLUDED.first_name, last_name = EXCLUDED.last_name, birthday = EXCLUDED.birthday"
      query = "INSERT INTO people (email, first_name, last_name, birthday) VALUES "
              ++ placeholders
              ++ " ON CONFLICT (email) DO "
              ++ updateStmt
  c <- conn
  _ <- run c query values
  commit c
  return ()
