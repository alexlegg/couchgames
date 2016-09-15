{-# LANGUAGE TemplateHaskell #-}
module CouchGames.Lobby (
      Lobby(..)
    , LobbyState(..)
    , GameType(..)
    ) where

import CouchGames.Player
import Data.Text
import Elm.Derive

data GameType = Resistance | Hanabi
    deriving (Show, Eq)

deriveBoth defaultOptions ''GameType

data LobbyState = LSOpen | LSClosed
    deriving (Show, Eq)

deriveBoth defaultOptions ''LobbyState

data Lobby = Lobby {
      lobbyId           :: Int
    , lobbyPlayers      :: [Player]
    , lobbyGame         :: GameType
    , lobbyState        :: LobbyState
    } deriving (Show, Eq)

deriveBoth defaultOptions ''Lobby
