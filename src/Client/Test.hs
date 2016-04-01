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

alert :: Text -> Fay ()
alert = ffi "alert(%1)"

addWindowEvent :: Text -> (Event -> Fay ()) -> Fay ()
addWindowEvent = ffi "window.addEventListener(%1, %2)"

main :: Fay ()
main = addWindowEvent "load" $ \_ -> do
    -- Initialise virtual dom
    let pm = UserRegister "" "" ""
    putPageModel pm
    initDom render pm

    sock <- connect "http://localhost:8080"

    on sock "connect" $ \_ -> do
        print "connected"
        emit sock "cookie" (SessionCookie "thisismycookie")

    on sock "player" $ \(Player id s n) -> do
        print $ append "Received player: " n
        print $ append "Received player: " (pack (show id))
        print $ append "Received player: " s

-- Model

data PageModel =
    UserRegister
        { urUsername       :: Text
        , urEmail          :: Text
        , urPassword       :: Text
        }
    | UserLogin
        { ulUsername       :: Text
        , ulPassword       :: Text
        }
    | FormSubmitted

-- Update

data Action =
      Register
    | Login

update :: Action -> PageModel -> PageModel
update Register pm  = FormSubmitted
update Login pm     = FormSubmitted

getPageModel :: Fay PageModel
getPageModel = ffi "window.pageModel"

putPageModel :: PageModel -> Fay ()
putPageModel = ffi "window.pageModel = %1"

dispatch :: Action -> (Event -> Fay ())
dispatch act = \evt -> do
    pm      <- getPageModel
    let pm' = update act pm
    updateDom render pm'
    putPageModel pm'

-- View

render :: PageModel -> VirtualDom
render (UserRegister{..}) =
    hdiv []
        [ table []
            [ tr []
                [ td [] [ ht "Username" ]
                , td [] 
                    [ input [ inputType "text", value urUsername ] ]
                ]
            , tr []
                [ td [] [ ht "Password" ]
                , td [] 
                    [ input [ inputType "password" , value urPassword ] ]
                ]
            , tr []
                [ td [colspan "2"]
                    [ input [ inputType "submit", onClick (dispatch Register) ] ]
                ]
            ]
        ]

render FormSubmitted =
    hdiv [] [ ht "Form submitted" ]

