{-# LANGUAGE OverloadedStrings #-}
module CouchGames.ResistanceSpec (resistanceSpec) where

import Test.Hspec
import Data.Functor.Identity
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
    ]

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
