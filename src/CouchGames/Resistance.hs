module CouchGames.Resistance
    ( Resistance
    , resistance
    , defaultConfig
    , GameConfig(..)
    , Action(..)
    , GameState(..)
    , PublicState(..)
    , HiddenState(..)
    , Role(..)
    , Mission(..)
    , MissionResult(..)
    ) where

import Data.Maybe 

import CouchGames.Player
import CouchGames.Util
import qualified CouchGames.Game as G

type Resistance = G.Game IO GameConfig GameState Action PublicState HiddenState

resistance :: Resistance
resistance = G.Game {
      G.initGame = initGame
    , G.storeGame = storeGame
    , G.gameState = gameState
    , G.playerState = playerState
    , G.gameAction = gameAction
    }

data GameConfig = GameConfig {
      commander :: Bool
    , bodyGuard :: Bool
    }
    deriving (Eq, Show)

defaultConfig :: GameConfig
defaultConfig = GameConfig { commander = False, bodyGuard = False }

data Action =
      ProposeMission [Player]
    | VoteOnProposal
    | GoOnMission
    | Assissinate 
    deriving (Eq, Show)

data ProposalVote = Accept | Reject
    deriving (Eq, Show)

type Proposal = ([Player], [(Player, ProposalVote)])

data MissionResult = Succeed | Fail
    deriving (Eq, Show)

data Mission = Mission {
      missionPlayers :: [Player]
    , missionTokens :: [(Player, MissionResult)]
    } deriving (Eq, Show)

type PublicMission = ([Player], Maybe MissionResult)

data Role = Resistance | Spy | Commander | BodyGuard | FalseCommander | Assassin
    deriving (Eq, Show)

data GamePhase = Propose | Vote | DoMission | Assassination | GameOver
    deriving (Eq, Show)

data GameState = GameState {
      config :: GameConfig
    , phase :: GamePhase
    , players :: [(Player, Role)]
    , leader :: Player
    , missions :: [Mission]
    , proposals :: [Proposal]
    } deriving (Eq, Show)

data PublicState = PublicState {
      pubPlayers :: [Player]
    , pubMissions :: [PublicMission]
    , pubProposals :: [Proposal]
    } deriving (Eq, Show)

data HiddenState = HiddenState {
      hiddenRoles :: [(Player, Role)]
    } deriving (Eq, Show)

generateRoles :: GameConfig -> Int -> [Role]
generateRoles config n = spies ++ falseCommanders ++ assassins ++ operatives ++ commanders ++ bodyGuards
    where
    numSpies = (n `quotCeil` 3) - 2
    spies = replicate numSpies Spy
    falseCommanders = if (bodyGuard config) then [FalseCommander] else [Spy]
    assassins = if (commander config) then [Assassin] else [Spy]
    operatives = replicate (n - numSpies - 4) Resistance
    commanders = if (commander config) then [Commander] else [Resistance]
    bodyGuards = if (bodyGuard config) then [BodyGuard] else [Resistance]

missionRequired :: Int -> Int -> Maybe Int 
missionRequired nPlayers mission
    | nPlayers < 0 || nPlayers > 10     = Nothing
    | mission < 0 || mission > 5        = Nothing
    | otherwise                         = Just $ (reqs !! (nPlayers - 5)) !! mission
    where reqs = [[2, 3, 2, 3, 3]
                , [2, 3, 4, 3, 4]
                , [2, 3, 3, 4, 4]
                , [3, 4, 4, 5, 5]
                , [3, 4, 4, 5, 5]
                , [3, 4, 4, 5, 5]
                , [4, 5, 5, 6, 6]
                , [4, 5, 5, 6, 6]]

initGame :: GameConfig -> [Player] -> IO (Either String GameState)
initGame conf ps 
    | length ps < 5     = return (Left "Wrong number of players")
    | length ps > 10    = return (Left "Wrong number of players")
    | otherwise         = do
        rs <- shuffle (generateRoles conf (length ps))
        lead <- selectRandom ps

        return $ Right $ GameState {
          config = conf
        , phase = Propose
        , players = zip ps rs
        , leader = lead
        , missions = []
        , proposals = []
        }

quotCeil :: Int -> Int -> Int
quotCeil x y
    | r > 0     = q + 1
    | r == 0    = q
    where (q, r) = quotRem x y

storeGame _ = return ()

gameState _ = return (PublicState [] [] [])

playerState _ = return (HiddenState [])

gameAction :: Player -> Action -> GameState -> IO (Either String GameState)
gameAction p (ProposeMission ps) g = return (proposeMission p ps g)
gameAction p _ g = return (Left "Not implemented")

missionComplete :: Mission -> Bool
missionComplete m = length (missionTokens m) == length (missionPlayers m)

currentMission :: GameState -> Int
currentMission g
    | null (missions g)                     = 0
    | missionComplete (last (missions g))   = length (missions g) - 1
    | otherwise                             = length (missions g)

proposeMission :: Player -> [Player] -> GameState -> Either String GameState
proposeMission p ps g
    | phase g /= Propose        = Left "Wrong game phase"
    | leader g /= p             = Left "Only the leader can propose a mission"
    | isNothing r               = Left "Bad game state"
    | length ps /= fromJust r   = Left "Wrong number of players in mission proposal"
    | otherwise                 = Right g { missions = missions g ++ [newMission]
                                          , phase = Vote }
    where
        m = currentMission g
        r = missionRequired (length (players g)) m
        newMission = Mission {missionPlayers = ps, missionTokens = []}
