{-# LANGUAGE TemplateHaskell #-}
module CouchGames.Resistance
    ( Resistance
    , resistance
    , defaultConfig
    , GameConfig(..)
    , Action(..)
    , GameState(..)
    , GamePhase(..)
    , PublicState(..)
    , HiddenState(..)
    , Role(..)
    , Mission(..)
    , MissionToken(..)
    , PublicMission(..)
    , Proposal(..)
    , ProposalVote(..)
    ) where

import Data.Maybe 
import Data.List
import Elm.Derive

import CouchGames.Player
import CouchGames.Util
import qualified CouchGames.Game as G

type Resistance = G.GameAPI IO GameConfig GameState Action PublicState HiddenState

resistance :: Resistance
resistance = G.GameAPI {
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
    | VoteOnProposal ProposalVote
    | GoOnMission MissionToken
    | Assissinate 
    deriving (Eq, Show)

data ProposalVote = Accept | Reject
    deriving (Eq, Show)

data Proposal = Proposal {
      proposalPlayers :: [Player] 
    , proposalVotes :: [(Player, ProposalVote)]
    } deriving (Eq, Show)

data MissionToken = Succeed | Fail
    deriving (Eq, Show)

data Mission = Mission {
      missionPlayers :: [Player]
    , missionTokens :: [(Player, MissionToken)]
    } deriving (Eq, Show)

data PublicMission = PublicMission [Player] (Maybe MissionToken)
    deriving (Eq, Show)

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
    } deriving (Eq)

instance Show GameState where
    show g = "-------------\n"
        ++ "Config: " ++ show (config g) ++ "\n"
        ++ "Phase: " ++ show (phase g) ++ "\n"
        ++ "Players: " ++ show (map (mapFst displayName) (players g)) ++ "\n"
        ++ "Leader: " ++ show (displayName (leader g)) ++ "\n"
        ++ "Missions: " ++ showMissions (missions g) ++ "\n"
        ++ "Proposals: " ++ showProposals (proposals g) ++ "\n"
        ++ "-------------"
        where
            showMission m = 
                "{" ++ show (map displayName (missionPlayers m))
                ++ ", tokens: " ++ show (map (mapFst displayName) (missionTokens m)) ++ "}"
            showMissions ms = intercalate "\n" (map showMission ms)
            showProposal p = 
                "{" ++ show (map displayName (proposalPlayers p))
                ++ ", votes: " ++ show (map (mapFst displayName) (proposalVotes p)) ++ "}"
            showProposals ps = intercalate "\n" (map showProposal ps)

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
gameAction p (VoteOnProposal v) g = return (voteOnProposal p v g)
gameAction p (GoOnMission t) g = return (goOnMission p t g)
gameAction p _ g = return (Left "Not implemented")

missionComplete :: Mission -> Bool
missionComplete m = length (missionTokens m) == length (missionPlayers m)

currentMission :: GameState -> Int
currentMission g
    | null (missions g)                     = 0
    | missionComplete (last (missions g))   = length (missions g)
    | otherwise                             = length (missions g) - 1

proposeMission :: Player -> [Player] -> GameState -> Either String GameState
proposeMission p ps g
    | phase g /= Propose        = Left "Wrong game phase"
    | leader g /= p             = Left "Only the leader can propose a mission"
    | isNothing r               = Left "Bad game state"
    | length ps /= fromJust r   = Left "Wrong number of players in mission proposal"
    | otherwise                 = Right g { proposals = proposals g ++ [Proposal ps []]
                                          , phase = Vote }
    where
        m = currentMission g
        r = missionRequired (length (players g)) m

voteOnProposal :: Player -> ProposalVote -> GameState -> Either String GameState
voteOnProposal p v g
    | phase g /= Vote       = Left "Wrong game phase"
    | null (proposals g)    = Left "Wrong game phase"
    | length votes == np    = Left "Wrong game phase"
    | isJust votedAlready   = Left "Player has already voted"
    | length votes == np-1  
    && accepts > rejects    = Right g { proposals = proposals'
                                      , missions = missions g ++ [mission']
                                      , phase = DoMission }
    | length votes == np-1  = Right g { proposals = proposals'
                                      , phase = Propose }
    | otherwise             = Right g { proposals = proposals' }
    where
        votes = proposalVotes (last (proposals g))
        np = length (players g)
        votedAlready = find ((==) p . fst) votes
        addVote prop = prop {proposalVotes = proposalVotes prop ++ [(p, v)]}
        accepts = length $ elemIndices Accept (v : map snd votes)
        rejects = length $ elemIndices Reject (v : map snd votes)
        proposals' = updateLast addVote (proposals g)
        mission' = Mission { missionPlayers = proposalPlayers (last (proposals g))
                           , missionTokens = [] }

goOnMission :: Player -> MissionToken -> GameState -> Either String GameState
goOnMission p t g
    | phase g /= DoMission          = Left "Wrong game phase"
    | null (missions g)             = Left "Wrong game phase"
    | not (p `elem` mPlayers)       = Left "Player is not on the mission"
    | p `elem` (map fst mTokens)    = Left "Player has already put in a token"
    | missionOver && gameOver       
    && succMissions == 3
    && commander (config g)         = Right g { missions = updateLast addToken (missions g)
                                              , phase = Assassination }
    | missionOver && gameOver       = Right g { missions = updateLast addToken (missions g)
                                              , phase = GameOver }
    | missionOver                   = Right g { missions = updateLast addToken (missions g)
                                              , phase = Propose
                                              , leader = nextLeader (leader g) (map fst (players g)) }
    | otherwise                     = Right g { missions = updateLast addToken (missions g) }
    where
        mission = last (missions g)
        mPlayers = missionPlayers mission
        mTokens = missionTokens mission
        addToken m = m { missionTokens = missionTokens m ++ [(p, t)] }
        missionOver = length mTokens == length mPlayers - 1
        failedMissions = length $ elemIndices Fail (map missionResult (missions g))
        succMissions = length $ elemIndices Succeed (map missionResult (missions g))
        gameOver = failedMissions == 3 || succMissions == 3

missionResult :: Mission -> MissionToken
missionResult m 
    | Fail `elem` (map snd (missionTokens m))   = Fail
    | otherwise                                 = Succeed

nextLeader :: Player -> [Player] -> Player
nextLeader curr ps = ps !! next index
    where 
        index           = elemIndex curr ps
        next (Just i)   = if i == length ps - 1 then 0 else i+1
        next Nothing    = error "Leader is not in the game"

deriveBoth defaultOptions ''MissionToken
deriveBoth defaultOptions ''PublicMission
deriveBoth defaultOptions ''ProposalVote
deriveBoth defaultOptions ''Proposal
deriveBoth defaultOptions ''PublicState
