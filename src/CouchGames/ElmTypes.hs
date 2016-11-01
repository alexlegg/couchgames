module CouchGames.ElmTypes
    ( writeElmModules
    ) where

import Data.Proxy
import Elm.Module
import CouchGames.Message
import CouchGames.Session
import CouchGames.Lobby
import CouchGames.Player
import qualified CouchGames.Resistance as R
import Elm.Derive
import Elm.Versions

writeElmModules :: IO ()
writeElmModules = do
    writeFile "clientsrc/Types.elm" $ makeElmModuleWithVersion Elm0p17 "Types"
        [ DefineElm (Proxy :: Proxy MessageFromClient)
        , DefineElm (Proxy :: Proxy MessageFromServer)

        , DefineElm (Proxy :: Proxy SessionCookie)
        , DefineElm (Proxy :: Proxy SessionRegister)
        , DefineElm (Proxy :: Proxy SessionLogin)

        , DefineElm (Proxy :: Proxy Lobby)
        , DefineElm (Proxy :: Proxy LobbyState)
        , DefineElm (Proxy :: Proxy GameType)

        , DefineElm (Proxy :: Proxy Player)

        , DefineElm (Proxy :: Proxy R.PublicMission)
        , DefineElm (Proxy :: Proxy R.MissionToken)
        , DefineElm (Proxy :: Proxy R.ProposalVote)
        , DefineElm (Proxy :: Proxy R.Proposal)
        , DefineElm (Proxy :: Proxy R.PublicState)
        ]
