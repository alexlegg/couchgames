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
    ) where

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
      ProposeMission
    | VoteOnProposal
    | GoOnMission
    | Assissinate 
    deriving (Eq, Show)

data ProposalVote = Accept | Reject
    deriving (Eq, Show)

type Proposal = ([Player], [(Player, Maybe ProposalVote)])

data MissionResult = Succeed | Fail
    deriving (Eq, Show)

type Mission = [(Player, Maybe MissionResult)]

type PublicMission = ([Player], Maybe MissionResult)

data Role = Resistance | Spy | Commander | BodyGuard | FalseCommander | Assassin
    deriving (Eq, Show)

data GameState = GameState {
      config :: GameConfig
    , players :: [(Player, Role)]
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

initGame :: GameConfig -> [Player] -> IO (Either String GameState)
initGame conf ps 
    | length ps < 5     = return (Left "Wrong number of players")
    | length ps > 10    = return (Left "Wrong number of players")
    | otherwise         = do
        let numSpies = length ps `quotCeil` 3
        let spies = replicate numSpies Spy
        let operatives = replicate (length ps - numSpies) Resistance
        rs <- shuffle (spies ++ operatives)

        return $ Right $ GameState {
          config = conf
        , players = zip ps rs
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
gameAction p a g = return (Right g)
