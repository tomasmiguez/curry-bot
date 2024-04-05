{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack (allChannels, channelByName, sendMsg, allUsersWithEmail, User(..)) where

import Data.Aeson
import Network.HTTP.Simple
import Data.String (IsString(fromString))
import Data.List (find)
import Data.ByteString (toStrict)
import Data.Maybe (fromJust, isJust)
import qualified Data.ByteString.Char8 as S8

import Config (slackConfig, SlackConfig(..))

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
    [ "channel" .= m.channelId
    , "text" .= m.text
    ]

data User = User
               { id :: !String
               , email :: !(Maybe String)
               } deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
    User <$> obj .: "id"
              <*> (obj .: "profile" >>= (.:? "email"))

newtype UsersResponse = UsersResponse
                      { users :: [User]
                      } deriving (Show, Eq)

instance FromJSON UsersResponse where
  parseJSON = withObject "UsersResponse" $ \obj ->
    UsersResponse <$> obj .: "members"

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
allChannels = slackConfig >>= allChannelsWithToken . (.token)
  where
    allChannelsWithToken token = do
      res <- httpJSON $ slackGet token "users.conversations"
      let resBody = getResponseBody res :: ChannelsResponse
      return resBody.channels

channelByName :: String -> IO Channel
channelByName n = slackConfig >>= channelByNameWithToken . (.token)
  where
    channelByNameWithToken token = do
      res <- httpJSON $ slackGet token "users.conversations"
      let resBody = getResponseBody res :: ChannelsResponse
      return $ fromJust $ find (\c -> n == c.name) resBody.channels

sendMsg :: String -> Channel -> IO ()
sendMsg m c = slackConfig >>= sendMsgWithToken . (.token)
  where
    sendMsgWithToken token = do
      res <- httpJSON $ slackPost token "chat.postMessage" Message { channelId = c.id, text = m }
      let resBody = getResponseBody res :: Value
      S8.putStrLn $ toStrict $ encode resBody

allUsers :: IO [User]
allUsers = slackConfig >>= allUsersWithToken . (.token)
  where
    allUsersWithToken token = do
      res <- httpJSON $ slackGet token "users.list"
      let resBody = getResponseBody res :: UsersResponse
      return resBody.users

allUsersWithEmail :: IO [User]
allUsersWithEmail = filter (isJust . (.email)) <$> allUsers
