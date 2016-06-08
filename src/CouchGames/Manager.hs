module CouchGames.Manager
    ( Manager
    , emptyManager
    , newUser
    , getUserFromSocket
    , newLobby
    , newPlayer
    ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Text as T
import           Data.Int
import           CouchGames.Player
import           CouchGames.Lobby

-- A user may have many players
-- A user may have many sockets
data Manager = Manager
    { users         :: Map.Map T.Text Int64 -- Sockets to User Ids
    , players       :: IMap.IntMap Player   -- User Ids to Players
    , sockets       :: IMap.IntMap [T.Text] -- Player Ids to Sockets
    , nextPlayerId  :: Int

    , nextLobbyId   :: Int
    , lobbies       :: IMap.IntMap Lobby
    }

emptyManager :: Manager
emptyManager = Manager
    { users = Map.empty
    , players = IMap.empty
    , sockets = IMap.empty
    , nextPlayerId = 1
    , nextLobbyId = 1
    , lobbies = IMap.empty
    }

newUser :: Int64 -> T.Text -> Manager -> Manager
newUser userId socketId m = m { users = Map.insert socketId userId (users m) }

getUserFromSocket :: T.Text -> Manager -> Maybe Int64
getUserFromSocket socketId m = Map.lookup socketId (users m)

newLobby :: GameType -> Manager -> (Int, Manager)
newLobby gameType m =
    (nextLobbyId m, m { lobbies = IMap.insert (nextLobbyId m) lobby (lobbies m), nextLobbyId = nextLobbyId m + 1 })
    where
        lobby = Lobby { lobbyPlayers = []
                      , lobbyGame = gameType
                      , lobbyState = LSOpen
                      }

newPlayer :: Int64 -> T.Text -> T.Text -> Int -> Manager -> (Int, Manager)
newPlayer userId userName socketId lobbyId m =
    (nextPlayerId m, m { players = players', sockets = sockets', nextPlayerId = nextPlayerId m + 1 })
    where
        players'    = IMap.insert (fromIntegral userId) p (players m)
        p           = Player { playerId     = nextPlayerId m
                             , displayName  = userName
                             }
        sockets'    = IMap.insert (nextPlayerId m) [socketId] (sockets m)

                                
