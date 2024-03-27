{-# LANGUAGE OverloadedStrings #-}

module Slack (listChannels) where

import Data.Aeson
import Network.HTTP.Simple

type ChannelId = String
type ChannelName = String

data Channel = Channel
  { id :: ChannelId
  , name :: ChannelName
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

listChannels :: IO (Maybe [Channel])
listChannels = do
  let request = setRequestBearerAuth "XXX"
              $ setRequestMethod "GET"
              $ setRequestPath "api/users.conversations"
              $ setRequestHost "slack.com"
              $ defaultRequest

  response <- httpJSON request

  let maybeResponseBody = getResponseBody response :: Maybe ChannelsResponse

  return $ channels <$> maybeResponseBody
