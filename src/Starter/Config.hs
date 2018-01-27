{-# LANGUAGE OverloadedStrings #-}

module Starter.Config
    ( Config(..)
    , loadConfig
    ) where

import Control.Monad (join)
import Data.Text     as T (Text, unpack, words)
import Data.Yaml     (FromJSON (..), Object, Parser, decodeFileEither, prettyPrintParseException,
                      withObject, (.!=), (.:), (.:?))

data Config = Config {
  cfgLaunchCommand     :: Text
  , cfgLaunchArgs      :: [Text]
  , cfgStartCommands   :: [Text]
  , cfgRestartCommands :: [Text]
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "starter configuration" parser
    where
      parser :: Object -> Parser Config
      parser v = do
        cmd <- v .:? "launch-command" .!= "stack ghci"
        case T.words cmd of
          []          -> fail $ "Expected launch-command to be a non-empty string, found " ++ unpack cmd
          (prog:args) -> Config
                         <$> return prog
                         <*> return args
                         <*> v .: "start-commands"
                         <*> v .: "restart-commands"

loadConfig :: FilePath -> IO Config
loadConfig file = join $ either (fail . prettyPrintParseException) return <$> decodeFileEither file
