{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Bamboo (allEmployees) where

import Data.Aeson
import Network.HTTP.Simple
import Data.String (IsString(fromString))
import Data.Time.Calendar
import Data.Aeson.Types

import Config (bambooConfig, BambooConfig(..))

data Employee = Employee
              { email :: !(Maybe String)
              , firstName :: !String
              , lastName :: !String
              , birthday :: !(Maybe Day)
              } deriving (Show)

instance FromJSON Employee where
  parseJSON = withObject "Employee" $ \obj ->
    Employee <$> obj .: "workEmail"
             <*> obj .: "firstName"
             <*> obj .: "lastName"
             <*> parseBirthday obj

parseBirthday :: Object -> Parser (Maybe Day)
parseBirthday obj = do
  date <- obj .: "dateOfBirth"
  if date == "0000-00-00"
    then return Nothing
    else Just <$> parseJSON (String date)

newtype EmployeesResponse = EmployeesResponse
                         { employees :: [Employee]
                         } deriving (Show)

instance FromJSON EmployeesResponse where
  parseJSON = withObject "EmployeesResponse" $ \obj ->
    EmployeesResponse <$> obj .: "employees"

bambooGet :: String -> String -> Request
bambooGet token resource = setRequestBasicAuth (fromString token) "x"
                         $ setRequestMethod "GET"
                         $ setRequestPath (fromString $ "api/gateway.php/fudo/v1/" ++ resource)
                         $ setRequestHost "api.bamboohr.com"
                         $ setRequestQueryString [("format", Just "json")]
                         $ defaultRequest

allEmployees :: IO [Employee]
allEmployees = bambooConfig >>= allChannelsWithToken . (.token)
  where
    allChannelsWithToken token = do
      reportId <- (.reportId) <$> bambooConfig
      res <- httpJSON $ bambooGet token ("reports/" ++ reportId)
      let resBody = getResponseBody res :: EmployeesResponse
      return resBody.employees
