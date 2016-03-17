{-# LANGUAGE EmptyDataDecls #-}
module SocketIO
    ( Connection
    , connect
    , on
    , emit
    ) where

import FFI

data Connection

connect :: String -> Fay Connection
connect = ffi "io(%1)"

on :: Connection -> String -> (a -> Fay ()) -> Fay ()
on = ffi "%1.on(%2, %3)"

emit :: Connection -> String -> a -> Fay ()
emit = ffi "%1.emit(%2, %3)"
