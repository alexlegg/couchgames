{-# LANGUAGE DeriveDataTypeable #-}
module CouchGames.Lobby (
      Lobby(..)
    , LobbyState(..)
    , createLobby
    , joinLobby
    , leaveLobby
    , closeLobby
    ) where

import Data.Data
import CouchGames.Player (PlayerId)

data Lobby = Lobby {
      lobbyPlayers      :: [PlayerId]
    , lobbyGame         :: Int
    , lobbyState        :: LobbyState
    } deriving (Show, Data)

data LobbyState = LSOpen | LSClosed deriving (Show, Data)

createLobby :: PlayerId -> Lobby
createLobby p = Lobby [p] 0 LSOpen

joinLobby :: Lobby -> PlayerId -> Lobby
joinLobby l p = l { lobbyPlayers = lobbyPlayers l ++ [p] }

leaveLobby :: Lobby -> PlayerId -> Maybe Lobby
leaveLobby l p = case (lobbyPlayers l) of
    []      -> Nothing
    (x:[])  -> Nothing
    xs      -> Just $ l { lobbyPlayers = delete p xs }

delete :: Eq a => a -> [a] -> [a]
delete _ []     = []
delete x (y:ys) = if x == y then ys else y : delete x ys

closeLobby :: Lobby -> Lobby
closeLobby l = l { lobbyState = LSClosed }
