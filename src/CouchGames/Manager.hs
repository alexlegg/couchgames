module CouchGames.Manager
    ( Manager
    , ManagerS
    , emptyManager
    , newUser
    , getUserFromSession
    , newLobby
    , getLobbies
    , newPlayer
    , getSockets
    ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Text as T
import           Data.Int
import           Control.Monad.State
import           CouchGames.Player
import           CouchGames.Lobby
import           Web.Users.Types
import           Network.WebSockets

-- A user may have one player
-- A player may have many sockets
data Manager = Manager
    { sessions      :: Map.Map T.Text (Int64, User) -- Session Ids to Users
    , players       :: IMap.IntMap Player           -- User Ids to Players
    , sockets       :: IMap.IntMap [Connection]     -- Player Ids to Sockets
    , nextPlayerId  :: Int

    , nextLobbyId   :: Int
    , lobbies       :: IMap.IntMap Lobby
    }

type ManagerS = State Manager

emptyManager :: Manager
emptyManager = Manager
    { sessions = Map.empty
    , players = IMap.empty
    , sockets = IMap.empty
    , nextPlayerId = 1
    , nextLobbyId = 1
    , lobbies = IMap.empty
    }

newUser :: Int64 -> User -> T.Text -> ManagerS ()
newUser uid user sessId = do
    m <- get
    put m { sessions = Map.insert sessId (uid, user) (sessions m) }

getUserFromSession :: T.Text -> ManagerS (Maybe (Int64, User))
getUserFromSession sessId = do
    m <- get
    return $ Map.lookup sessId (sessions m)

newLobby :: GameType -> ManagerS Int
newLobby gameType = do
    m <- get
    put m { lobbies = IMap.insert (nextLobbyId m) (lobby (nextLobbyId m)) (lobbies m), nextLobbyId = nextLobbyId m + 1 }
    return $ nextLobbyId m 
    where
        lobby id = Lobby { lobbyId = id
                         , lobbyPlayers = []
                         , lobbyGame = gameType
                         , lobbyState = LSOpen
                         }

getLobbies :: ManagerS [Lobby]
getLobbies = do
    m <- get
    return $ IMap.elems (lobbies m)

newPlayer :: Int64 -> T.Text -> Connection -> Int -> ManagerS Int
newPlayer userId userName socket lobbyId = do
    m <- get
    put m { players = players' m, sockets = sockets' m, nextPlayerId = nextPlayerId m + 1 }
    return $ nextPlayerId m
    where
        players' m  = IMap.insert (fromIntegral userId) (p m) (players m)
        p m         = Player { playerId     = nextPlayerId m
                             , displayName  = userName
                             }
        sockets' m  = IMap.insert (nextPlayerId m) [socket] (sockets m)

                                
getSockets :: ManagerS [Connection]
getSockets = do
    m <- get
    return $ concat $ IMap.elems (sockets m)
