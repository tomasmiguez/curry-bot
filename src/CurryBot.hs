{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module CurryBot (updateSlackIds, updateEmployees) where

import Data.Maybe (fromJust)

import Slack
import BotDb
import Bamboo

updateSlackIds :: IO ()
updateSlackIds = do
  users <- allUsersWithEmail
  mapM_ (\u -> updateSlackIdByEmail u.id $ fromJust u.email) users

updateEmployees :: IO ()
updateEmployees = do
  employees <- allEmployees
  upsertEmployees employees
