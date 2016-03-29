{-# LANGUAGE DeriveDataTypeable #-}
module CouchGames.Player (
      PlayerId
    , Player(..)
    , DumbType(..)
    ) where

import Data.Data
import Data.Text

data Player = Player
    { userId        :: Int
    , socketId      :: Text
    , displayName   :: Text
    } deriving (Data)

type PlayerId = Int

data DumbType = DumbType
    { dumbInt       :: Int
    } deriving (Data, Read, Show, Typeable)
