{-# LANGUAGE OverloadedStrings #-}

module Lib (getConnString) where

import Data.Map
import Data.Text (Text)
import Data.Yaml (ParseException, decodeFileEither)

type Config = Map Text (Map Text String)

getConnString :: IO (Either ParseException String)
getConnString = do
  parsedFile <- decodeFileEither "config/config.yaml" :: IO (Either ParseException Config)
  return $ buildConnString . (! "db") <$> parsedFile

buildConnString :: Map Text String -> String
buildConnString config = foldr1 (++) ["postgresql://", user, "@", host, ":", port, "/", database]
  where
    user = config ! "user"
    host = config ! "host"
    port = config ! "port"
    database = config ! "database"
