{-# LANGUAGE DeriveDataTypeable #-}
module CouchGames.Session (
      SessionCookie(..)
    ) where

import Data.Data
import Data.Text

data SessionCookie      = SessionCookie Text deriving (Data, Typeable)

data SessionRegister    = SessionRegister
    { regUsername       :: Text
    , regPassword       :: Text
    , regEmail          :: Text
    } deriving (Data, Typeable)
