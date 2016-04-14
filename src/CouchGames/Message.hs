{-# LANGUAGE TemplateHaskell #-}
module CouchGames.Message
    ( MessageFromServer(..)
    , MessageFromClient(..)
    ) where

import Data.Text
import Elm.Derive
import CouchGames.Session

data MessageFromServer
    = MsgSessionId SessionCookie
    | MsgBadLogin
    | MsgBadRegister Text
    deriving (Show, Eq)

deriveBoth defaultOptions ''MessageFromServer

data MessageFromClient
    = MsgRegister SessionRegister
    | MsgLogin SessionLogin
    | MsgCookie SessionCookie
    deriving (Show, Eq)

deriveBoth defaultOptions ''MessageFromClient
