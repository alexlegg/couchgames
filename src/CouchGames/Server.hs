{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module CouchGames.Server (runApp, app) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import           Web.Users.Types
import           Web.Users.Postgresql ()
import           Web.Spock.Safe
import           Database.PostgreSQL.Simple
import qualified CouchGames.Config as C

app' userBackend = spockT id $ do
    get "/" $ do
        text "hello"

    get "/some-json" $ do
        json $ object ["foo" .= Number 23, "bar" .= Number 42]

---    S.post "user/register" $ do
---        json 

app :: IO Application
app = do
    config      <- C.parseConfig "config.yaml"
    conn        <- connectToDatabase config
    userBackend <- initUserBackend conn
    spockAsApp (app' userBackend)

runApp :: IO ()
runApp = do
    config      <- C.parseConfig "config.yaml"
    conn        <- connectToDatabase config
    userBackend <- initUserBackend conn
    runSpock 80 (app' userBackend)

connectToDatabase (Just config) = do
    connect defaultConnectInfo
        { connectDatabase   = C.database config
        , connectUser       = C.dbUser config
        , connectPassword   = C.dbPassword config }

