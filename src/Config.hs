{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config (getConnString, getSlackToken) where

import Data.Yaml (ParseException, decodeFileEither, FromJSON)
import GHC.Generics

data Config = Config
            { dbConfig :: !DbConfig
            , slackConfig :: !SlackConfig
            } deriving (Show, Generic)

instance FromJSON Config

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

config :: IO (Either ParseException Config)
config = decodeFileEither "config/config.yaml"

getConnString :: IO (Either ParseException String)
getConnString = fmap (buildConnString . dbConfig) <$> config

buildConnString :: DbConfig -> String
buildConnString c = foldr1 (++) ["postgresql://", user c, "@", host c, ":", port c, "/", database c]

getSlackToken :: IO (Either ParseException String)
getSlackToken = fmap (token . slackConfig) <$> config
