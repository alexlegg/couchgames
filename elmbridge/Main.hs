{-# LANGUAGE TemplateHaskell #-}
import Elm.Derive
import Elm.Module
import Data.Proxy
import CouchGames.Session

deriveBoth defaultOptions ''SessionCookie

deriveBoth defaultOptions ''SessionRegister

deriveBoth defaultOptions ''SessionLogin

main :: IO ()
main =
    putStrLn $ makeElmModule "Types"
    [ DefineElm (Proxy :: Proxy SessionCookie)
    , DefineElm (Proxy :: Proxy SessionRegister)
    , DefineElm (Proxy :: Proxy SessionLogin)
    ]
