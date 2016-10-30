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

nextLeader :: Int -> Player -> Player
nextLeader n lead
    | lead == p1            = p2
    | lead == p2            = p3
    | lead == p3            = p4
    | lead == p4            = p5
    | n == 5 && lead == p5  = p1
    | lead == p5            = p6
    | n == 6 && lead == p6  = p1
    | lead == p6            = p7
    | n == 7 && lead == p7  = p1
    | lead == p7            = p8
    | n == 8 && lead == p8  = p1
    | lead == p8            = p9
    | n == 9 && lead == p9  = p1
    | lead == p9            = p10
    | lead == p10           = p1

leaders :: Int -> Player -> [Player]
leaders n lead = lead : leaders n (nextLeader n lead)

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
        Left err -> do
            putStrLn (show p)
            putStrLn (show a)
            putStrLn (show g)
            expectationFailure $ "An action failed unexpectedly (" ++ err ++ ")"
        Right gs -> f gs

withActions :: R.GameState -> [(Player, R.Action)] -> (R.GameState -> Expectation) -> Expectation
withActions g [] f          = f g
withActions g ((p, a):as) f = withAction g p a (\g -> withActions g as f)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

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
                g1 <- (gameAction R.resistance) notLead (R.ProposeMission [p1, p2]) gs
                g1 `shouldBe` (Left "Only the leader can propose a mission")

        it "Won't allow proposal during voting" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                withAction gs lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    g2 <- (gameAction R.resistance) lead (R.ProposeMission [p1, p2]) g1
                    g2 `shouldBe` Left "Wrong game phase"

        it "Allows a mission proposal" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                withAction gs lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    length (R.proposals g1) `shouldBe` 1
                    let (proposed, votes) = last (R.proposals g1)
                    proposed `shouldContain` [p1, p2]
                    votes  `shouldBe` []


        it "Won't allow voting before a proposal" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let p = head fivePlayers
                g1 <- (gameAction R.resistance) p (R.VoteOnProposal R.Reject) gs
                g1 `shouldBe` Left "Wrong game phase"

        it "Allows voting" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                withAction gs lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    let p = head fivePlayers
                    withAction g1 p (R.VoteOnProposal R.Reject) $ \g2 -> do
                        fst (head (snd (head (R.proposals g2)))) `shouldBe` p
                        snd (head (snd (head (R.proposals g2)))) `shouldBe` R.Reject

        it "Won't allow double voting" $ do
            withNewGame fivePlayers R.defaultConfig $ \gs -> do
                let lead = R.leader gs
                let p = head fivePlayers
                withAction gs lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    withAction g1 p (R.VoteOnProposal R.Reject) $ \g2 -> do
                        g3 <- (gameAction R.resistance) p (R.VoteOnProposal R.Reject) g2
                        g3 `shouldBe` Left "Player has already voted"

        it "Rejects a proposal on a minority" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                let lead = R.leader g0
                let votes = [ (fivePlayers !! 0, R.Reject)
                            , (fivePlayers !! 1, R.Reject)
                            , (fivePlayers !! 2, R.Reject)
                            , (fivePlayers !! 3, R.Accept)
                            , (fivePlayers !! 4, R.Accept)]
                let actions = map (mapSnd R.VoteOnProposal) votes

                withAction g0 lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    withActions g1 actions $ \g2 -> do
                        R.phase g2 `shouldBe` R.Propose
                        R.missions g2 `shouldBe` []
                        fst (last (R.proposals g2)) `shouldContain` [p1, p2]
                        snd (last (R.proposals g2)) `shouldContain` votes

        it "Accepts a proposal on a majority" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                let lead = R.leader g0
                let votes = [ (fivePlayers !! 0, R.Reject)
                            , (fivePlayers !! 1, R.Accept)
                            , (fivePlayers !! 2, R.Reject)
                            , (fivePlayers !! 3, R.Accept)
                            , (fivePlayers !! 4, R.Accept)]
                let actions = map (mapSnd R.VoteOnProposal) votes

                withAction g0 lead (R.ProposeMission [p1, p2]) $ \g1 -> do
                    withActions g1 actions $ \g2 -> do
                        R.phase g2 `shouldBe` R.DoMission
                        R.missionPlayers (last (R.missions g2)) `shouldContain` [p1, p2]
                        R.missionTokens (last (R.missions g2)) `shouldBe` []
                        fst (last (R.proposals g2)) `shouldContain` [p1, p2]
                        snd (last (R.proposals g2)) `shouldContain` votes

        it "Won't allow putting two tokens in for a mission" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                let lead = R.leader g0
                let votes = [ (fivePlayers !! 0, R.Accept)
                            , (fivePlayers !! 1, R.Accept)
                            , (fivePlayers !! 2, R.Accept)
                            , (fivePlayers !! 3, R.Accept)
                            , (fivePlayers !! 4, R.Accept)]
                let vActions = map (mapSnd R.VoteOnProposal) votes
                let actions = (lead, R.ProposeMission [p1, p2]) 
                            : vActions ++ [(p1, R.GoOnMission R.Succeed)]

                withActions g0 actions $ \g1 -> do
                    R.phase g1 `shouldBe` R.DoMission
                    fst (last (R.proposals g1)) `shouldContain` [p1, p2]
                    snd (last (R.proposals g1)) `shouldContain` votes

                    g2 <- (gameAction R.resistance) p1 (R.GoOnMission R.Succeed) g1
                    g2 `shouldBe` Left "Player has already put in a token"

        it "Allows succeeding a mission" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                let lead = R.leader g0
                let votes = [ (fivePlayers !! 0, R.Accept)
                            , (fivePlayers !! 1, R.Accept)
                            , (fivePlayers !! 2, R.Accept)
                            , (fivePlayers !! 3, R.Accept)
                            , (fivePlayers !! 4, R.Accept)]
                let tokens = [ (p1, R.Succeed)
                             , (p2, R.Succeed)]
                let actions = (lead, R.ProposeMission [p1, p2]) 
                            : (map (mapSnd R.VoteOnProposal) votes)
                            ++ (map (mapSnd R.GoOnMission) tokens)

                withActions g0 actions $ \g1 -> do
                    R.phase g1 `shouldBe` R.Propose

        it "Allows failing a mission" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                let lead = R.leader g0
                let votes = [ (fivePlayers !! 0, R.Accept)
                            , (fivePlayers !! 1, R.Accept)
                            , (fivePlayers !! 2, R.Accept)
                            , (fivePlayers !! 3, R.Accept)
                            , (fivePlayers !! 4, R.Accept)]
                let tokens = [ (p1, R.Succeed)
                             , (p2, R.Fail)]
                let actions = (lead, R.ProposeMission [p1, p2]) 
                            : (map (mapSnd R.VoteOnProposal) votes)
                            ++ (map (mapSnd R.GoOnMission) tokens)

                withActions g0 actions $ \g1 -> do
                    R.phase g1 `shouldBe` R.Propose

        it "Won't allow a Resistance player to fail a mission" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                True `shouldBe` False

        it "Allows the Resistance to win" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                let lead = R.leader g0
                let missions = [[p1, p2], [p1, p2, p3], [p1, p2]]
                let proposals = zip (leaders 5 lead) (map R.ProposeMission missions)
                let votes = map (\p -> (p, R.VoteOnProposal R.Accept)) fivePlayers
                let tokens = map (map (\p -> (p, R.GoOnMission R.Succeed))) missions

                let actions = concat $ zipWith (\p t -> p : votes ++ t) proposals tokens

                withActions g0 actions $ \g1 -> do
                    R.phase g1 `shouldBe` R.GameOver

        it "Allows the Spies to win" $ do
            withNewGame fivePlayers R.defaultConfig $ \g0 -> do
                let lead = R.leader g0
                let missions = [[p1, p2], [p1, p2, p3], [p1, p2]]
                let proposals = zip (leaders 5 lead) (map R.ProposeMission missions)
                let votes = map (\p -> (p, R.VoteOnProposal R.Accept)) fivePlayers
                let tokens = map (map (\p -> (p, R.GoOnMission R.Fail))) missions

                let actions = concat $ zipWith (\p t -> p : votes ++ t) proposals tokens

                withActions g0 actions $ \g1 -> do
                    R.phase g1 `shouldBe` R.GameOver
