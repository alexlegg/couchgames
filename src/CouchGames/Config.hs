{-# LANGUAGE DeriveGeneric #-}
module CouchGames.Config (Config(..), parseConfig) where

import GHC.Generics
import Data.Yaml

data Config = Config
    { database          :: String
    , dbUser            :: String
    , dbPassword        :: String
    } deriving (Show, Eq, Generic)

instance FromJSON Config

parseConfig :: FilePath -> IO (Maybe Config)
parseConfig = decodeFile
