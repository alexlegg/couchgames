{-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax, RecordWildCards #-}
module Hello where

import Prelude
import FFI
import SocketIO
import VirtualDOM
import Data.Text

import CouchGames.Player
import CouchGames.Lobby
import CouchGames.Session

data Event

alert :: Text -> Fay ()
alert = ffi "alert(%1)"

addWindowEvent :: Text -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

main :: Fay ()
main = do
    print "Hello Console2!"
    addWindowEvent "load" $ \evt -> do
        -- Initialise virtual dom
        dom <- createElement (render (PageUserRegister "" "" ""))
        newRootNode dom

        sock <- connect "http://localhost:8080"

        on sock "connect" $ \_ -> do
            print "connected"
            emit sock "cookie" (SessionCookie "thisismycookie")


        emit sock "join lobby" "test"

        on sock "player" $ \(Player id s n) -> do
            print $ append "Received player: " n
            print $ append "Received player: " (pack (show id))
            print $ append "Received player: " s

        return ()


data PageState =
    PageUserRegister
        { purUsername       :: Text
        , purEmail          :: Text
        , purPassword       :: Text
        }
    | PageUserLogin
        { pulUsername       :: Text
        , pulPassword       :: Text
        }

render :: PageState -> VirtualDom
render (PageUserRegister{..}) =
    h "div" []
        [ h "table" []
            [ h "tr" []
                [ h "td" [] [ ht "Username" ]
                , h "td" [] 
                    [ (h "input" [ ("type", "text"), ("value", purUsername) ] []) ]
                ]
            , h "tr" []
                [ h "td" [] [ ht "Password" ]
                , h "td" [] 
                    [ (h "input" [ ("type", "password"), ("value", purPassword) ] []) ]
                ]
            ]
        ]

getVTree :: Fay VirtualDom
getVTree = ffi "window.virtualTree"

putVTree :: VirtualDom -> Fay ()
putVTree = ffi "window.virtualTree = %1"

newRootNode :: Dom -> Fay ()
newRootNode = ffi "window.rootNode = document.body.appendChild(%1)"

putRootNode :: Dom -> Fay ()
putRootNode = ffi "window.rootNode = %1"

getRootNode :: Fay Dom
getRootNode = ffi "window.rootNode"

updateDom :: PageState -> Fay ()
updateDom ps = do
    t       <- getVTree
    let t'  = render ps
    let p   = diff t t'
    root    <- getRootNode
    root'   <- patch root p
    putRootNode root'
    putVTree t'

