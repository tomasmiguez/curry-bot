{-# LANGUAGE OverloadedStrings #-}

module Slack (allChannels, channelByName, sendMsg) where

import Prelude hiding (id)

import Data.Aeson
import Network.HTTP.Simple
import Data.String (IsString(fromString))
import Data.List (find)

import Config (slackToken)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (toStrict)
import Data.Maybe (fromJust)

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

data Message = Message
  { channelId :: !ChannelId
  , text :: !String
  } deriving (Show, Eq)

instance ToJSON Message where
  toJSON m = object
    [ "channel" .= channelId m
    , "text" .= text m
    ]

slackGet :: String -> String -> Request
slackGet token resource = setRequestBearerAuth (fromString token)
                        $ setRequestMethod "GET"
                        $ setRequestPath (fromString $ "api/" ++ resource)
                        $ setRequestHost "slack.com"
                        $ defaultRequest

slackPost :: ToJSON j => String -> String -> j -> Request
slackPost t r j = setRequestBearerAuth (fromString t)
                $ setRequestMethod "POST"
                $ setRequestPath (fromString $ "api/" ++ r)
                $ setRequestHost "slack.com"
                $ setRequestBodyJSON j
                $ defaultRequest

allChannels :: IO [Channel]
allChannels = slackToken >>= allChannelsWithToken
  where
    allChannelsWithToken token = do
      res <- httpJSON $ slackGet token "users.conversations"
      let resBody = getResponseBody res :: ChannelsResponse
      return $ channels resBody

channelByName :: String -> IO Channel
channelByName n = slackToken >>= channelByNameWithToken
  where
    channelByNameWithToken token = do
      res <- httpJSON $ slackGet token "conversations.list"
      let resBody = getResponseBody res :: ChannelsResponse
      return $ fromJust $ find (\c -> n == name c) $ channels resBody

sendMsg :: String -> Channel -> IO ()
sendMsg m c = slackToken >>= sendMsgWithToken
  where
    sendMsgWithToken token = do
      res <- httpJSON $ slackPost token "chat.postMessage" Message { channelId = id c, text = m }
      let resBody = getResponseBody res :: Value
      S8.putStrLn $ toStrict $ JSON.encode resBody
