{-# LANGUAGE EmptyDataDecls #-}
module Hello where

import FFI
import SocketIO
import VirtualDOM

import CouchGames.Player
import CouchGames.Lobby

data Event

alert :: String -> Fay ()
alert = ffi "alert(%1)"

setBodyHtml :: Dom -> Fay ()
setBodyHtml = ffi "document.body.appendChild(%1)"

setBodyHtml' :: String -> Fay ()
setBodyHtml' = ffi "document.body.innerHTML = %1"

addWindowEvent :: String -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

greet :: Event -> Fay ()
greet event = do
  putStrLn "The document has loaded"
  setBodyHtml' "Hello HTML!"

main :: Fay ()
main = do
    putStrLn "Hello Console2!"
    addWindowEvent "load" $ \evt -> do
        sock <- connect "http://localhost:8080"

        on sock "player" $ \(Player id s n) -> do
            putStrLn $ "Received player: " ++ n
            putStrLn $ "Received player: " ++ (show id)
            putStrLn $ "Received player: " ++ s
        
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
