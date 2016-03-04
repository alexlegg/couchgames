{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TypeFamilies #-}
module CouchGames.Server (runApp, appTest, getConfig, connectToDatabase, flushDatabase) where

import           Data.Aeson (Value(..), object, (.=))
import           Data.Int
import           Data.Maybe
import qualified Data.Text as T
import           Network.Wai (Application, Middleware)
import           Network.HTTP.Types.Status
import           Web.Users.Types
import           Web.Users.Postgresql ()
import           Web.Spock.Safe
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple
import qualified CouchGames.Config as C

app' :: Connection -> IO Middleware
app' conn = spockT id $ do
    get "/" $ do
        text "hello"

    get "/some-json" $ do
        json $ object ["foo" .= Number 23, "bar" .= Number 42]

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

{- End User API -}

-- This is for testing only.
appTest :: Connection -> IO Application
appTest conn = do
    initUserBackend conn
    spockAsApp (app' conn)

getConfig :: IO (Maybe C.Config)
getConfig = C.parseConfig "config.yaml"

flushDatabase :: Connection -> IO ()
flushDatabase = destroyUserBackend

runApp :: IO ()
runApp = do
    config      <- C.parseConfig "config.yaml"
    conn        <- connectToDatabase config
    initUserBackend conn
    runSpock 80 (app' conn)

connectToDatabase (Just config) = do
    connect defaultConnectInfo
        { connectDatabase   = C.database config
        , connectUser       = C.dbUser config
        , connectPassword   = C.dbPassword config }

