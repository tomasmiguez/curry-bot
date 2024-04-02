{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (connStr, slackToken) where

import Data.Yaml
import GHC.Generics

data Config = Config
            { dbConfig :: !DbConfig
            , slackConfig :: !SlackConfig
            } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "db"
    <*> v .: "slack"

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

config :: IO Config
config = decodeFileThrow "config/config.yaml"

connStr :: IO String
connStr = buildConnString . dbConfig <$> config

buildConnString :: DbConfig -> String
buildConnString c = foldr1 (++) ["postgresql://", user c, "@", host c, ":", port c, "/", database c]

slackToken :: IO String
slackToken = token . slackConfig <$> config
