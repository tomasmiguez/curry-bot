{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (connStr, slackConfig, SlackConfig(..), bambooConfig, BambooConfig(..), ignoredBirthdays) where

import Data.Yaml
import GHC.Generics
import System.Directory

data Config = Config
            { dbConfig :: !DbConfig
            , slackConfig :: !SlackConfig
            , bambooConfig :: !BambooConfig
            , ignoredBirthdays :: ![String]
            } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "db"
    <*> v .: "slack"
    <*> v .: "bamboo"
    <*> v .: "ignoredBirthdays"

data DbConfig = DbConfig
              { user :: !String
              , host :: !String
              , port :: !String
              , database :: !String
              } deriving (Show, Generic)

instance FromJSON DbConfig

data SlackConfig = SlackConfig
                 { token :: !String
                 , channelName :: !String
                 } deriving (Show, Generic)

instance FromJSON SlackConfig

data BambooConfig = BambooConfig
                 { token :: !String
                 , reportId :: !String
                 } deriving (Show, Generic)

instance FromJSON BambooConfig

config :: IO Config
config = do
  home <- getHomeDirectory
  decodeFileThrow (home ++ "/.config/curry_bot/config.yaml")

connStr :: IO String
connStr = buildConnString . (.dbConfig) <$> config

buildConnString :: DbConfig -> String
buildConnString c = foldr1 (++) ["postgresql://", c.user, "@", c.host, ":", c.port, "/", c.database]

slackConfig :: IO SlackConfig
slackConfig = (.slackConfig) <$> config

bambooConfig :: IO BambooConfig
bambooConfig = (.bambooConfig) <$> config

ignoredBirthdays :: IO [String]
ignoredBirthdays = (.ignoredBirthdays) <$> config
