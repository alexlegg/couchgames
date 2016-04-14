{-# LANGUAGE TemplateHaskell #-}
module CouchGames.Session
    ( SessionCookie(..)
    , SessionRegister(..)
    , SessionLogin(..)
    ) where

import Data.Text
import Elm.Derive

data SessionCookie      = SessionCookie Text deriving (Show, Eq)

deriveBoth defaultOptions ''SessionCookie

data SessionRegister    = SessionRegister
    { regUsername       :: Text
    , regEmail          :: Text
    , regPassword       :: Text
    } deriving (Show, Eq)

deriveBoth defaultOptions ''SessionRegister

data SessionLogin       = SessionLogin
    { loginUsername     :: Text
    , loginPassword     :: Text
    } deriving (Show, Eq)

deriveBoth defaultOptions ''SessionLogin
