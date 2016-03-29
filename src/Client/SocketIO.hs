{-# LANGUAGE EmptyDataDecls #-}
module SocketIO
    ( Connection
    , connect
    , on
    , emit
    ) where

import FFI
import Data.Text

data Connection

connect :: Text -> Fay Connection
connect = ffi "io(%1)"

on :: Connection -> Text -> (a -> Fay ()) -> Fay ()
on = ffi "%1.on(%2, %3)"

emit :: Connection -> Text -> a -> Fay ()
emit = ffi "%1.emit(%2, %3)"
