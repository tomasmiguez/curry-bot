{-# LANGUAGE OverloadedStrings #-}

module Slack (allChannels, channelByName) where

import Data.Aeson
import Network.HTTP.Simple
import Data.String (IsString(fromString))
import Data.List (find)

import Config (getSlackToken)

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

allChannels :: IO (Maybe [Channel])
allChannels = getSlackToken >>= either (const $ return Nothing) allChannelsWithToken
  where
    allChannelsWithToken token = do
      response <- httpJSON $ slackGet token "users.conversations"
      let maybeResponseBody = getResponseBody response :: Maybe ChannelsResponse
      return $ channels <$> maybeResponseBody

channelByName :: String -> IO (Maybe Channel)
channelByName n = getSlackToken >>= either (const $ return Nothing) (channelByNameWithToken n)
  where
    channelByNameWithToken n' t = do
      r <- httpJSON $ slackGet t "conversations.list"
      let maybeBody = getResponseBody r :: Maybe ChannelsResponse
      return $ find (\c -> n' == name c) . channels =<< maybeBody
