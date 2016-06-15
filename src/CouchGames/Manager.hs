module CouchGames.Manager
    ( Manager
    , ManagerS
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
import           Control.Monad.State
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

type ManagerS = State Manager

emptyManager :: Manager
emptyManager = Manager
    { users = Map.empty
    , players = IMap.empty
    , sockets = IMap.empty
    , nextPlayerId = 1
    , nextLobbyId = 1
    , lobbies = IMap.empty
    }

newUser :: Int64 -> T.Text -> ManagerS ()
newUser userId socketId = do
    m <- get
    put m { users = Map.insert socketId userId (users m) }

getUserFromSocket :: T.Text -> ManagerS (Maybe Int64)
getUserFromSocket socketId = do
    m <- get
    return $ Map.lookup socketId (users m)

newLobby :: GameType -> ManagerS Int
newLobby gameType = do
    m <- get
    put m { lobbies = IMap.insert (nextLobbyId m) lobby (lobbies m), nextLobbyId = nextLobbyId m + 1 }
    return $ nextLobbyId m 
    where
        lobby = Lobby { lobbyPlayers = []
                      , lobbyGame = gameType
                      , lobbyState = LSOpen
                      }

newPlayer :: Int64 -> T.Text -> T.Text -> Int -> ManagerS Int
newPlayer userId userName socketId lobbyId = do
    m <- get
    put m { players = players' m, sockets = sockets' m, nextPlayerId = nextPlayerId m + 1 }
    return $ nextPlayerId m
    where
        players' m  = IMap.insert (fromIntegral userId) (p m) (players m)
        p m         = Player { playerId     = nextPlayerId m
                             , displayName  = userName
                             }
        sockets' m  = IMap.insert (nextPlayerId m) [socketId] (sockets m)

                                
