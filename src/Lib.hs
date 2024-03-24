{-# LANGUAGE OverloadedStrings #-}

module Lib (getConnString) where

import Data.Yaml (decodeFileEither, ParseException)
import  Data.Text (Text)
import Data.Map

type Config = Map Text (Map Text String)

getConnString :: IO (Either ParseException String)
getConnString = do
  parsedFile <- decodeFileEither "config/config.yaml" :: IO (Either ParseException Config)
  case parsedFile of
    Left x             -> return (Left x)
    Right parsedConfig -> do
      let dbConfig = parsedConfig ! "db"

          user = dbConfig ! "user"
          host = dbConfig ! "host"
          port = dbConfig ! "port"
          database = dbConfig ! "database"

          connString = foldr1 (++) ["postgresql://", user, "@", host, ":", port, "/", database]
      return (Right connString)
