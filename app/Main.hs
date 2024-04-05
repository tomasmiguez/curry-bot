module Main (main) where

import System.Environment (getArgs)

import CurryBot (sendTodayBirthdayReminder, updateSlackIds, updateEmployees)

main :: IO ()
main = run . head =<< getArgs

run :: String -> IO ()
run "import-bamboo"   = updateEmployees
run "update-slack"    = updateSlackIds
run "update-people"   = do
  _ <- updateEmployees
  _ <- updateSlackIds
  return ()
run "remind-birthday" = sendTodayBirthdayReminder
run _                 = putStrLn usage

usage :: String
usage = "Usage: curry-bot <command>.\n\
        \Possible commands are:\n\
        \import-bamboo: Queries Bamboo for people and upserts the list.\n\
        \update-slack: Updates Slack IDs of already existing people.\n\
        \update-people: Both previous commands.\n\
        \remind-birthday: Sends a birthday reminder for the current day."
