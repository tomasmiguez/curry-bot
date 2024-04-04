module BotDb (peopleBirthdayToday, updateSlackIdByEmail) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.Time.Calendar
import Data.Time.LocalTime

import Config (connStr)
import Person
import Control.Monad (when)

conn :: IO Connection
conn = connStr >>= connectPostgreSQL

peopleByBirthday :: Day -> IO [Person]
peopleByBirthday d = do
  c <- conn
  r <- quickQuery' c "SELECT slack_id, email, first_name, last_name, birthday FROM people where birthday = ?" [toSql d]
  return $ map rowToPeople r
  where
    rowToPeople [sqlSlackId, sqlEmail, sqlBirthday] =
      Person { slackId = fromSql sqlSlackId, email = fromSql sqlEmail, birthday = fromSql sqlBirthday }
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
