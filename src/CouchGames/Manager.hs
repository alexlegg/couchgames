module CouchGames.Manager
    ( Manager
    , ManagerS
    , emptyManager
    , newUser
    , getUserFromSession
    , newLobby
    , joinLobby
    , getLobbies
    , newPlayer
    , getSockets
    , removeSocket
    ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Text as T
import qualified Data.List as L
import           Data.Int
import           Control.Monad.State
import           CouchGames.Player
import           CouchGames.Lobby
import           CouchGames.Game
import qualified CouchGames.Resistance as R
import           Web.Users.Types
import           Network.WebSockets

data Game
    = GameResistance R.GameState

-- A user may have one player
-- A player may have many sockets
data Manager = Manager
    { sessions      :: Map.Map T.Text (Int64, User) -- Session Ids to Users
    , sockets       :: IMap.IntMap [Connection]     -- Player Ids to Sockets
    , nextPlayerId  :: Int
    , players       :: IMap.IntMap Player           -- User Ids to Players
    , nextLobbyId   :: Int
    , lobbies       :: IMap.IntMap Lobby            -- Lobby Id to Lobby
    , nextGameId    :: Int
    , games         :: IMap.IntMap Game             -- Game Id to Game
    }

type ManagerS = State Manager

emptyManager :: Manager
emptyManager = Manager
    { sessions = Map.empty
    , sockets = IMap.empty
    , nextPlayerId = 1
    , players = IMap.empty
    , nextLobbyId = 1
    , lobbies = IMap.empty
    , nextGameId = 1
    , games = IMap.empty
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

joinLobby :: Int -> Int -> ManagerS ()
joinLobby playerId lobbyId = do
    m <- get
    case (IMap.lookup playerId (players m)) of
        (Just p) ->
            put m { lobbies = IMap.adjust (addPlayer p) lobbyId (lobbies m) }
        Nothing ->
            return ()

getLobbies :: ManagerS [Lobby]
getLobbies = do
    m <- get
    return $ IMap.elems (lobbies m)

newPlayer :: Int64 -> T.Text -> Connection -> ManagerS Int
newPlayer userId userName socket = do
    m <- get
    put m { players = players' m, sockets = sockets' m, nextPlayerId = nextPlayerId m + 1 }
    return $ nextPlayerId m
    where
        players' m  = IMap.insert (fromIntegral userId) (p m) (players m)
        p m         = Player { playerId     = nextPlayerId m
                             , displayName  = userName
                             }
        sockets' m  = IMap.insert (nextPlayerId m) [socket] (sockets m)

-- For now just removes all sockets from a player
removeSocket :: Int -> ManagerS ()
removeSocket playerId = do
    m <- get
    case (IMap.lookup playerId (sockets m)) of
        Just pid -> put m { sockets = IMap.adjust (const []) playerId (sockets m) }
        Nothing -> return ()
                                
getSockets :: ManagerS [Connection]
getSockets = do
    m <- get
    return $ concat $ IMap.elems (sockets m)
