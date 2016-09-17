{-# LANGUAGE OverloadedStrings, QuasiQuotes, DataKinds, TypeFamilies, FlexibleContexts, ExistentialQuantification, DeriveDataTypeable, DeriveGeneric #-}
module CouchGames.Server 
    ( runApp
    , appTest
    , getConfig
    , connectToDatabase
    , flushDatabase
    ) where

import           Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import           Data.Int
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Network.Wai (Application, Middleware)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types.Status
import           Web.Users.Types
import           Web.Users.Postgresql ()
import           Web.Spock.Safe
import           Control.Monad.IO.Class
import           Control.Monad (forever)
import           Control.Exception (IOException, Exception, SomeException, catch, toException, finally)
import qualified Control.Exception as E
import qualified Control.Monad.State as MS
import           Database.PostgreSQL.Simple
import qualified CouchGames.Config as C
import           Network.Wai.Middleware.Static
import qualified Network.EngineIO.Wai as EIOWai
import qualified Control.Concurrent.STM as STM
import qualified Data.Vector as V
import           Data.Data
import           GHC.Generics
import           Control.Monad.State (MonadState)
import           Control.Monad.Reader (MonadReader, ask)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           System.Log.Logger
import           Data.String.Conversions
import qualified Network.WebSockets as WS
import           Network.Wai.Handler.WebSockets

import qualified CouchGames.Manager as M
import           CouchGames.Player
import           CouchGames.Lobby
import           CouchGames.Session
import           CouchGames.Message

app' :: Connection -> SpockT IO ()
app' conn = do
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
        file "text/html" "static/index.html"

    get "/some-json" $ do
        json $ object ["foo" .= Number 23, "bar" .= Number 42]

    get getUser $ \userId -> do
        u <- liftIO $ getUserById conn userId
        case u of
            (Just user) -> json $ object ["userId" .= userId, "name" .= u_name user]
            Nothing     -> sendError 1 "User not found"

    get getUsers $ do
        us <- liftIO $ listUsers conn Nothing (SortDesc UserFieldName)
        json $ map (\(uid, user) -> object ["userId" .= uid, "name" .= u_name user]) us

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

mkCouchUser :: T.Text -> T.Text -> T.Text -> User
mkCouchUser name email password = User
    { u_name        = name
    , u_email       = email
    , u_password    = makePassword (PasswordPlain password)
    , u_active      = True
    }

getUser :: Path '[Int64]
getUser = "users" <//> var

getUsers :: Path '[]
getUsers = "users"

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
    mgr         <- STM.newTVarIO M.emptyManager
    webApp      <- spockAsApp (spockT id (app' conn))
    run 8080 $ websocketsOr WS.defaultConnectionOptions (sockApp conn mgr) webApp

connectToDatabase (Just config) = do
    connect defaultConnectInfo
        { connectDatabase   = C.database config
        , connectUser       = C.dbUser config
        , connectPassword   = C.dbPassword config }

sockApp :: Connection -> STM.TVar M.Manager -> WS.ServerApp
sockApp dbConn manager pending = do
    -- liftIO $ putStrLn (show (WS.pendingRequest pending)) -- TODO: could
    -- use this for faster auth via cookies
    conn <- WS.acceptRequest pending
    emit conn $ MsgConnected
    forever $ do
        msg <- WS.receiveData conn
        case (Aeson.decode msg) of
            Just x  -> do
                u <- handleLogin conn dbConn manager x
                case u of
                    Just (uid, user, cookie) -> do
                        playerId <- withManager manager $ M.newPlayer uid (u_name user) conn
                        emit conn (MsgSession (SessionCookie cookie) (u_name user))
                        handlePlayer conn dbConn manager playerId
                    Nothing ->
                        return ()
            Nothing -> do
                liftIO $ errorM "Server" "Got a message from client that we couldn't parse:"
                liftIO $ errorM "Server" (show msg)

handleLogin :: WS.Connection -> Connection -> STM.TVar M.Manager -> MessageFromClient -> IO (Maybe (Int64, User, T.Text))
handleLogin sock conn mgr (MsgRegister (SessionRegister n e p)) = do
    u <- createUser conn (mkCouchUser n e p)
    case u of
        Left InvalidPassword -> do
            emit sock $ MsgBadRegister "Invalid password"
            return Nothing
        Left UsernameAlreadyTaken -> do
            emit sock $ MsgBadRegister "Username already taken"
            return Nothing
        Left UsernameAndEmailAlreadyTaken -> do
            emit sock $ MsgBadRegister "Username already taken"
            return Nothing
        Left EmailAlreadyTaken -> do
            emit sock $ MsgBadRegister "Email already taken"
            return Nothing
        Right uid ->
            handleLogin sock conn mgr (MsgLogin (SessionLogin n p))
        _ -> do
            emit sock $ MsgBadRegister "User registration failed"
            return Nothing

handleLogin sock conn mgr (MsgLogin (SessionLogin username password)) = do
    s <- liftIO $ authUser conn username (PasswordPlain password) (60 * 60 * 24 * 365)
    case s of
        Nothing -> do
            liftIO $ infoM "Server" (T.unpack username ++ " failed login.")
            emit sock MsgBadLogin
            return Nothing
        Just sessionId -> do
            liftIO $ infoM "Server" (T.unpack username ++ " logged in. Granted session id: " ++ T.unpack (unSessionId sessionId))

            -- Handle login by passing it off as a session auth
            handleLogin sock conn mgr (MsgCookie (SessionCookie (unSessionId sessionId)))

handleLogin sock conn mgr (MsgCookie (SessionCookie cookie)) = do
    sess <- liftIO $ verifySession conn (SessionId cookie) (60 * 60 * 24 * 365)
    case sess of
        Nothing -> do
            liftIO $ infoM "Server" ("Bad cookie: " ++ T.unpack cookie)
            emit sock MsgBadCookie
            return Nothing
        Just uid -> do
            u <- liftIO $ getUserById conn uid
            case u of
                Nothing -> do
                    liftIO $ errorM "Server" ("Good cookie, bad user: " ++ T.unpack cookie ++ " " ++ show uid)
                    emit sock MsgBadCookie
                    return Nothing
                Just user -> do
                    liftIO $ infoM "Server" (T.unpack (u_name user) ++ " authorised with session id: " ++ T.unpack cookie)
                    _ <- withManager mgr $ M.newUser uid user cookie
                    return $ Just (uid, user, cookie)

handleLogin _ _ _ _ = do
    errorM "Server" "Bad message before login"
    return Nothing

handlePlayer :: WS.Connection -> Connection -> STM.TVar M.Manager -> Int -> IO ()
handlePlayer sock conn mgr playerId = do
    putStrLn $ "connect " ++ show playerId
    broadcastLobbies mgr
    flip finally (disconnect mgr playerId) $
        forever $ do
            msg <- WS.receiveData sock
            case (Aeson.decode msg) of
                Just x  -> do
                    handleMessage sock conn mgr playerId x
                Nothing -> do
                    liftIO $ errorM "Server" "Got a message from client that we couldn't parse:"
                    liftIO $ errorM "Server" (show msg)

disconnect :: STM.TVar M.Manager -> Int -> IO ()
disconnect mgr playerId = do
    withManager mgr $ M.removeSocket playerId
    putStrLn $ "disconnect " ++ (show playerId)

handleMessage :: WS.Connection -> Connection -> STM.TVar M.Manager -> Int -> MessageFromClient -> IO ()
handleMessage sock conn mgr playerId (MsgNewGame sessId gameType) = do
    putStrLn "MsgNewGame"
    _ <- withManager mgr $ M.newLobby gameType 

    putStrLn "broadcast"
    broadcastLobbies mgr

handleMessage _ _ _ _ _ = do
    errorM "Server" "Bad message after login"

broadcastLobbies :: STM.TVar M.Manager -> IO ()
broadcastLobbies mgr = do
    lobbies <- withManager mgr M.getLobbies
    liftIO $ putStrLn (show lobbies)
    broadcast mgr (MsgLobbyList lobbies)

broadcast :: STM.TVar M.Manager -> MessageFromServer -> IO ()
broadcast mgr msg = do
    socks <- withManager mgr M.getSockets
    liftIO $ putStrLn (show (length socks))
    mapM_ (\s -> emit s msg) socks

emit :: WS.Connection -> MessageFromServer -> IO ()
emit sock msg = WS.sendTextData sock (Aeson.encode msg)

withManager :: STM.TVar M.Manager -> (M.ManagerS a) -> IO a
withManager manager f = 
    STM.atomically $ do
        m <- STM.readTVar manager
        let (a, m') = MS.runState f m
        STM.writeTVar manager m'
        return a
