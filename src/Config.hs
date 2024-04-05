{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (connStr, slackToken, bambooConfig, BambooConfig(..)) where

import Data.Yaml
import GHC.Generics
import System.Directory

data Config = Config
            { dbConfig :: !DbConfig
            , slackConfig :: !SlackConfig
            , bambooConfig :: !BambooConfig
            } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "db"
    <*> v .: "slack"
    <*> v .: "bamboo"

data DbConfig = DbConfig
              { user :: !String
              , host :: !String
              , port :: !String
              , database :: !String
              } deriving (Show, Generic)

instance FromJSON DbConfig

newtype SlackConfig = SlackConfig
                 { token :: String
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

slackToken :: IO String
slackToken = (.token) . (.slackConfig) <$> config

bambooConfig :: IO BambooConfig
bambooConfig = (.bambooConfig) <$> config
