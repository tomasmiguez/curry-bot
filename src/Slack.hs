{-# LANGUAGE OverloadedStrings #-}

module Slack (allChannels, channelByName, sendMsg) where

import Prelude hiding (id)

import Data.Aeson
import Network.HTTP.Simple
import Data.String (IsString(fromString))
import Data.List (find)

import Config (getSlackToken)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (toStrict)
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)

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

allChannels :: IO (Maybe [Channel])
allChannels = getSlackToken >>= either (const $ return Nothing) allChannelsWithToken
  where
    allChannelsWithToken token = do
      response <- httpJSON $ slackGet token "users.conversations"
      let maybeResponseBody = getResponseBody response :: Maybe ChannelsResponse
      return $ channels <$> maybeResponseBody

channelByName :: String -> IO (Maybe Channel)
channelByName n = getSlackToken >>= either (const $ return Nothing) channelByNameWithToken
  where
    channelByNameWithToken t = do
      r <- httpJSON $ slackGet t "conversations.list"
      let maybeBody = getResponseBody r :: Maybe ChannelsResponse
      return $ find (\c -> n == name c) . channels =<< maybeBody

sendMsg :: String -> Channel -> IO ()
sendMsg m c = getSlackToken >>= either (const $ return ()) sendMsgWithToken
  where
    sendMsgWithToken t = do
      r <- httpJSON $ slackPost t "chat.postMessage" Message { channelId = id c, text = m }
      let maybeBody = getResponseBody r :: Maybe Value
      when (isJust maybeBody) $ do
        S8.putStrLn $ toStrict $ JSON.encode $ fromJust maybeBody
