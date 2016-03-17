{-# LANGUAGE DeriveDataTypeable #-}
module CouchGames.Player (
      PlayerId
    , Player(..)
    ) where

import Data.Data

data Player = Player
    { userId        :: Int
    , socketId      :: String
    , displayName   :: String
    } deriving (Show, Data)

type PlayerId = Int
