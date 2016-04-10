{-# LANGUAGE TemplateHaskell #-}
module CouchGames.Message
    ( MessageFromServer(..)
    , MessageFromClient(..)
    ) where

import Data.Text
import Elm.Derive
import CouchGames.Session

data MessageFromServer
    = MsgRegistered SessionCookie
    deriving (Show, Eq)

deriveBoth defaultOptions ''MessageFromServer

data MessageFromClient
    = MsgRegister SessionRegister
    | MsgCookie SessionCookie
    deriving (Show, Eq)

deriveBoth defaultOptions ''MessageFromClient
