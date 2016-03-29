{-# LANGUAGE EmptyDataDecls, OverloadedStrings, RebindableSyntax #-}
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

setBodyHtml :: Dom -> Fay ()
setBodyHtml = ffi "document.body.appendChild(%1)"

setBodyHtml' :: Text -> Fay ()
setBodyHtml' = ffi "document.body.innerHTML = %1"

addWindowEvent :: Text -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

greet :: Event -> Fay ()
greet event = do
  print "The document has loaded"
  setBodyHtml' "Hello HTML!"

main :: Fay ()
main = do
    print "Hello Console2!"
    addWindowEvent "load" $ \evt -> do
        sock <- connect "http://localhost:8080"

        on sock "connect" $ \_ -> do
            print "connected"
            emit sock "cookie" (DumbType 123)


        emit sock "join lobby" "test"

        on sock "player" $ \(Player id s n) -> do
            print $ append "Received player: " n
            print $ append "Received player: " (pack (show id))
            print $ append "Received player: " s
        
        let inp     = h "input" [("type", "text"), ("value", "foo")] []
        let h1      = h "h1" [] [VirtualText "Test!"]
        let tree    = h "div" [] [VirtualTree inp, VirtualTree h1]
        dom         <- createElement tree
        setBodyHtml dom

        addWindowEvent "click" $ \_ -> do
            let tree'   = h "div" [] [VirtualTree h1, VirtualTree inp]
            let p       = diff tree tree'
            dom'        <- patch dom p
            return ()

        return ()

