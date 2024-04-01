{-# LANGUAGE OverloadedStrings #-}

module Slack (listChannels) where

import Data.Aeson
import Network.HTTP.Simple
import Config (getSlackToken)
import Data.String (IsString(fromString))

type ChannelId = String
type ChannelName = String

data Channel = Channel
  { id :: !ChannelId
  , name :: !ChannelName
  } deriving (Show, Eq)

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \obj ->
    Channel <$> obj .: "id"
            <*> obj .: "name"

newtype ChannelsResponse = ChannelsResponse
  { channels :: [Channel]
  } deriving (Show, Eq)

instance FromJSON ChannelsResponse where
  parseJSON = withObject "ChannelsResponse" $ \obj ->
    ChannelsResponse <$> obj .: "channels"

slackGet :: String -> String -> Request
slackGet token resource = setRequestBearerAuth (fromString token)
                        $ setRequestMethod "GET"
                        $ setRequestPath (fromString $ "api/" ++ resource)
                        $ setRequestHost "slack.com"
                        $ defaultRequest

listChannels :: IO (Maybe [Channel])
listChannels = getSlackToken >>= either (const $ return Nothing) listChannelsWithToken
  where
    listChannelsWithToken token = do
      response <- httpJSON $ slackGet token "users.conversations"
      let maybeResponseBody = getResponseBody response :: Maybe ChannelsResponse
      return $ channels <$> maybeResponseBody
