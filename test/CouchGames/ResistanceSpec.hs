{-# LANGUAGE OverloadedStrings #-}
module CouchGames.ResistanceSpec (resistanceSpec) where

import Test.Hspec
import Data.Either
import Control.Monad

import CouchGames.Game
import CouchGames.Player
import qualified CouchGames.Resistance as R

comOnlyConfig = R.GameConfig { R.commander = True, R.bodyGuard = False }
bodyGuardConfig = R.GameConfig { R.commander = True, R.bodyGuard = True }

p1 = Player 1 "Player1"
p2 = Player 2 "Player2"
p3 = Player 3 "Player3"
p4 = Player 4 "Player4"
p5 = Player 5 "Player5"
p6 = Player 6 "Player6"
p7 = Player 7 "Player7"
p8 = Player 8 "Player8"
p9 = Player 9 "Player9"
p10 = Player 10 "Player10"
p11 = Player 11 "Player11"

fivePlayers = [p1, p2, p3, p4, p5]
sixPlayers = [p1, p2, p3, p4, p5, p6]
sevenPlayers = [p1, p2, p3, p4, p5, p6, p7]
eightPlayers = [p1, p2, p3, p4, p5, p6, p7, p8]
ninePlayers = [p1, p2, p3, p4, p5, p6, p7, p8, p9]
tenPlayers = [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10]

playersToRoles = [ 
    (R.defaultConfig, fivePlayers, 
        [R.Resistance, R.Resistance, R.Resistance, R.Spy, R.Spy])
    , (R.defaultConfig, sixPlayers, 
        [R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Spy, R.Spy])
    , (R.defaultConfig, sevenPlayers, 
        [R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Spy, R.Spy, R.Spy])
    , (R.defaultConfig, eightPlayers, 
        [R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Spy, R.Spy, R.Spy])
    , (R.defaultConfig, ninePlayers, 
        [R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Spy, R.Spy, R.Spy])
    , (R.defaultConfig, tenPlayers, 
        [R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Spy, R.Spy, R.Spy, R.Spy])
    , (comOnlyConfig, fivePlayers, 
        [R.Commander, R.Resistance, R.Resistance, R.Assassin, R.Spy])
    , (comOnlyConfig, sixPlayers, 
        [R.Commander, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.Spy])
    , (comOnlyConfig, sevenPlayers, 
        [R.Commander, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.Spy, R.Spy])
    , (comOnlyConfig, eightPlayers, 
        [R.Commander, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.Spy, R.Spy])
    , (comOnlyConfig, ninePlayers, 
        [R.Commander, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.Spy, R.Spy])
    , (comOnlyConfig, tenPlayers, 
        [R.Commander, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.Spy, R.Spy, R.Spy])
    , (bodyGuardConfig, fivePlayers, 
        [R.Commander, R.BodyGuard, R.Resistance, R.Assassin, R.FalseCommander])
    , (bodyGuardConfig, sixPlayers, 
        [R.Commander, R.BodyGuard, R.Resistance, R.Resistance, R.Assassin, R.FalseCommander])
    , (bodyGuardConfig, sevenPlayers, 
        [R.Commander, R.BodyGuard, R.Resistance, R.Resistance, R.Assassin, R.FalseCommander, R.Spy])
    , (bodyGuardConfig, eightPlayers, 
        [R.Commander, R.BodyGuard, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.FalseCommander, R.Spy])
    , (bodyGuardConfig, ninePlayers, 
        [R.Commander, R.BodyGuard, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.FalseCommander, R.Spy])
    , (bodyGuardConfig, tenPlayers, 
        [R.Commander, R.BodyGuard, R.Resistance, R.Resistance, R.Resistance, R.Resistance, R.Assassin, R.FalseCommander, R.Spy, R.Spy])
    ]

withNewGame :: [Player] -> R.GameConfig -> (R.GameState -> Expectation) -> Expectation
withNewGame ps config f = do
    g <- (initGame R.resistance) config ps
    case g of
        Left err -> expectationFailure "Failed to init game"
        Right gs -> f gs

withAction :: R.GameState -> Player -> R.Action -> (R.GameState -> Expectation) -> Expectation
withAction g p a f = do
    g' <- (gameAction R.resistance) p a g
    case g' of
        Left err -> expectationFailure "An action failed unexpectedly"
        Right gs -> f gs

resistanceSpec = do

    describe "Resistance" $ do
        it "fails to make games with the wrong number of players" $ do
            g1 <- (initGame R.resistance) R.defaultConfig []
            g1 `shouldBe` (Left "Wrong number of players")
            g2 <- (initGame R.resistance) R.defaultConfig [p1, p2, p3, p4] 
            g2 `shouldBe` (Left "Wrong number of players")
            g3 <- (initGame R.resistance) R.defaultConfig [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11] 
            g3 `shouldBe` (Left "Wrong number of players")

        it "creates games with the correct roles" $ do
            forM_ playersToRoles $ \(config, players, roles) -> do
                g <- (initGame R.resistance) config players
                case g of
                    Left err -> expectationFailure "Failed to init game"
                    Right gs -> (map snd (R.players gs)) `shouldMatchList` roles

        it "Won't allow proposal of the wrong number of players" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                g1 <- (gameAction R.resistance) lead (R.ProposeMission []) gs
                g1 `shouldBe` (Left "Wrong number of players in mission proposal")

                g2 <- (gameAction R.resistance) lead (R.ProposeMission [p1]) gs
                g2 `shouldBe` (Left "Wrong number of players in mission proposal")

                g3 <- (gameAction R.resistance) lead (R.ProposeMission [p1, p2, p3]) gs
                g3 `shouldBe` (Left "Wrong number of players in mission proposal")

        it "Won't allow proposal by a player that's not the leader" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                let notLead = if lead == p1 then p2 else p1
                withAction gs notLead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    g1 `shouldBe` (Left "Wrong game phase")

        it "Allows a mission proposal" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                withAction gs lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    length (R.missions g1) `shouldBe` 1
                    R.missionPlayers (head (R.missions g1)) `shouldContain` [p1, p2]
                    R.missionTokens (head (R.missions g1)) `shouldBe` []

        it "Won't allow proposal during voting" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                withAction gs lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    g2 <- (gameAction R.resistance) lead (R.ProposeMission [p1, p2]) g1
                    g2 `shouldBe` Left "Wrong game phase"


---        it "Won't allow voting before a proposal" $ do

