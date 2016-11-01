{-# LANGUAGE TemplateHaskell #-}
module CouchGames.Message
    ( MessageFromServer(..)
    , MessageFromClient(..)
    ) where

import Data.Text
import Elm.Derive
import CouchGames.Session
import CouchGames.Lobby
import qualified CouchGames.Resistance as R

data MessageFromServer
    = MsgConnected
    | MsgSession SessionCookie Text
    | MsgBadCookie
    | MsgBadLogin
    | MsgBadRegister Text
    | MsgLobbyList [Lobby]
    | MsgResistanceState R.PublicState
    deriving (Show, Eq)

deriveBoth defaultOptions ''MessageFromServer

data MessageFromClient
    = MsgRegister SessionRegister
    | MsgLogin SessionLogin
    | MsgCookie SessionCookie
    | MsgNewGame Text GameType
    deriving (Show, Eq)

deriveBoth defaultOptions ''MessageFromClient
