module CouchGames.ElmTypes
    ( writeElmModules
    ) where

import Data.Proxy
import Elm.Module
import CouchGames.Message
import CouchGames.Session
import Elm.Derive

writeElmModules :: IO ()
writeElmModules = do
    putStrLn (show defaultOptions)
    writeFile "clientsrc/Types.elm" $ makeElmModule "Types"
        [ DefineElm (Proxy :: Proxy MessageFromClient)
        , DefineElm (Proxy :: Proxy MessageFromServer)

        , DefineElm (Proxy :: Proxy SessionCookie)
        , DefineElm (Proxy :: Proxy SessionRegister)
        , DefineElm (Proxy :: Proxy SessionLogin)
        ]
