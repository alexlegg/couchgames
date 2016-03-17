{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TypeFamilies, FlexibleContexts #-}
module CouchGames.Server (runApp, appTest, getConfig, connectToDatabase, flushDatabase) where

import           Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import           Data.Int
import           Data.Maybe
import qualified Data.Text as T
import           Network.Wai (Application, Middleware)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types.Status
import           Web.Users.Types
import           Web.Users.Postgresql ()
import           Web.Spock.Safe
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple
import qualified CouchGames.Config as C
import           Network.Wai.Middleware.Static
import qualified Network.EngineIO.Wai as EIOWai
import qualified Control.Concurrent.STM as STM
import qualified Network.SocketIO as SocketIO
import           Fay.Convert

import           CouchGames.Player

app' :: Connection -> SpockT IO ()
app' conn = do
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
        file "text/html" "static/index.html"

    get "/some-json" $ do
        json $ object ["foo" .= Number 23, "bar" .= Number 42]

    get "/socket.io" $ middlewarePass
    post "/socket.io" $ middlewarePass

    get getUser $ \userId -> do
        u <- liftIO $ (getUserById conn userId :: IO (Maybe CouchUser))
        case u of
            (Just user) -> json $ object ["userId" .= userId, "name" .= u_name user]
            Nothing     -> sendError 1 "User not found"

    post newUser $ do
        userName        <- param "name"
        userEmail       <- param "email"
        userPassword    <- param "password"
        case (userName, userEmail, userPassword) of
            (Just n, Just e, Just p)    -> do
                u <- liftIO $ createUser conn (mkCouchUser n e p)
                case u of
                    Left InvalidPassword        -> sendError 3 "Invalid password"
                    Left UsernameAlreadyTaken   -> sendError 4 "Username already taken"
                    Left EmailAlreadyTaken      -> sendError 5 "Email already taken"
                    Right uid                   -> json $ object ["userId" .= uid, "name" .= n]
                    _                           -> sendError 6 "Unspecified error"
            _                                   -> sendError 2 "Missing parameters"

    post loginUser $ do
        userName        <- param "name"
        userPassword    <- param "password"
        case (userName, userPassword) of
            (Just n, Just p)    -> do
                s <- liftIO $ authUser conn n (PasswordPlain p) (60 * 60 * 24 * 365)
                case s of
                    Just sessId -> json $ object ["sessionId" .= sessId]
                    Nothing     -> sendError 7 "Bad login"
            _                   -> sendError 7 "Bad login"

sendError errorCode msg = do
    setStatus notFound404
    json $ object ["code" .= Number errorCode, "message" .= String msg]

{- User API -}

type CouchUser = User ()

mkCouchUser :: T.Text -> T.Text -> T.Text -> CouchUser
mkCouchUser name email password = User
    { u_name        = name
    , u_email       = email
    , u_password    = makePassword (PasswordPlain password)
    , u_active      = True
    , u_more        = ()
    }

getUser :: Path '[Int64]
getUser = "users" <//> var

newUser :: Path '[]
newUser = "users" <//> "register" 

loginUser :: Path '[]
loginUser = "users" <//> "login"

{- End User API -}

-- This is for testing only.
appTest :: Connection -> IO Application
appTest conn = do
    initUserBackend conn
    spockAsApp (spockT id (app' conn))

getConfig :: IO (Maybe C.Config)
getConfig = C.parseConfig "config.yaml"

flushDatabase :: Connection -> IO ()
flushDatabase = destroyUserBackend

-- Runs Spock as middleware wrapped around the SocketIO WAI Application
runApp :: IO ()
runApp = do
    config      <- C.parseConfig "config.yaml"
    conn        <- connectToDatabase config
    initUserBackend conn
    state       <- ServerState <$> STM.newTVarIO 0
    sock        <- SocketIO.initialize EIOWai.waiAPI (server state)
    web         <- spockT id (app' conn)
    run 8080 (web (EIOWai.toWaiApplication sock))

connectToDatabase (Just config) = do
    connect defaultConnectInfo
        { connectDatabase   = C.database config
        , connectUser       = C.dbUser config
        , connectPassword   = C.dbPassword config }

data ServerState = ServerState (STM.TVar Int)

server state = do
    liftIO $ putStrLn "server"
    let p = Player 2 "sdfsdf" "name"

    SocketIO.on "test" $ \(Something x) -> do
        liftIO $ putStrLn "recvd test"
        liftIO $ putStrLn (show x)
        SocketIO.emit "player" (showToFay p)
        SocketIO.emit "testing" (Something "abc")

data Something = Something { something :: T.Text }

instance Aeson.ToJSON Something where
    toJSON (Something i) = Aeson.object [ "something" .= i ]

instance Aeson.FromJSON Something where
    parseJSON = Aeson.withObject "something" $ \s ->
                    Something <$> s .: "something"
