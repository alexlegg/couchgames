{-# LANGUAGE TemplateHaskell #-}
module CouchGames.Player (
      Player(..)
    ) where

import Data.Int
import Data.Text
import Elm.Derive

data Player = Player
    { playerId      :: Int
    , displayName   :: Text
    } deriving (Show, Eq)

deriveBoth defaultOptions ''Player
