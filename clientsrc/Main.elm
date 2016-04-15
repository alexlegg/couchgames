import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import SocketIO exposing (io, emit, on)
import WebAPI.Cookie
import Dict
import StartApp
import Result
import Effects
import Login
import Signal
import Task exposing (Task, andThen)
import Util exposing (pipeMaybe)
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import Types as T

app = 
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs =
            [ Signal.map SocketConnected connectedMB.signal
            , Signal.map (SocketMsg << decodeMessage) incomingMB.signal
            , Signal.map Cookies cookieMB.signal
            ]
        }

decodeMessage s =
    let
        dec = decodeString T.jsonDecMessageFromServer
    in
        case dec s of
            Result.Ok msg ->
                Result.Ok msg
            Result.Err err ->
                -- Dirty hack to fix the case when we just get a string
                Result.formatError (\e -> e ++ " in \"" ++ s ++ "\"") (dec ("\"" ++ s ++ "\""))

main = 
    app.html

port run : Signal (Task Effects.Never ())
port run = app.tasks

cookieMB =
    Signal.mailbox Dict.empty

port getCookie : Task WebAPI.Cookie.Error ()
port getCookie =
    WebAPI.Cookie.get `andThen` Signal.send cookieMB.address

-- SocketIO handling

socket =
    io "http://localhost:8080" SocketIO.defaultOptions

connectedMB =
    Signal.mailbox False

port connected : Task x ()
port connected =
    socket `andThen` SocketIO.connected connectedMB.address

incomingMB =
    Signal.mailbox "null"

port incoming : Task x ()
port incoming =
    socket `andThen` SocketIO.on "MessageFromServer" incomingMB.address

emit : T.MessageFromClient -> Effects.Effects Action
emit msg = 
    socket
    `andThen` SocketIO.emit "MessageFromClient" 
        (encode 0 (T.jsonEncMessageFromClient msg))
    |> Task.map (always SocketSent)
    |> Effects.task

-- Model

type PageState = LogIn | Lobby | Game

type alias Model =
    { dbgOut        : String
    , pageState     : PageState
    , loginModel    : Login.Model
    , sessionId     : String
    , username      : String
    , socketConn    : Bool
    }

init : (Model, Effects.Effects Action)
init = 
    ( 
        { dbgOut = ""
        , pageState = LogIn
        , loginModel = Login.init
        , sessionId = ""
        , username = ""
        , socketConn = False
        }
    , Effects.none
    )

-- View

view : Signal.Address Action -> Model -> Html
view address model =
    case model.pageState of
        LogIn ->
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , text model.dbgOut
                , Html.br [] []
                , Login.view (Signal.forwardTo address LoginAction) model.loginModel ]
        Lobby ->
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , text model.dbgOut
                , Html.br [] []
                , text ("Logged in as " ++ model.username)
                ]
        Game ->
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , text model.dbgOut
                , Html.br [] []
                ]

-- Update

type Action
    = SocketConnected Bool
    | SocketMsg (Result String T.MessageFromServer)
    | SocketSent
    | Cookies (Dict.Dict String String)
    | LoginAction Login.Action
    | LoginSubmit String String
    | RegisterSubmit String String

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    SocketConnected conn ->
        ( { model | socketConn = conn }
        , Effects.none
        )
    SocketSent ->
        ( model, Effects.none )
    SocketMsg (Result.Ok msg) ->
        handleMessage msg model
    SocketMsg (Result.Err err) ->
        ( { model | dbgOut = "Error: " ++ err }, Effects.none )
    Cookies cookies ->
        ( model
        , case Dict.get "sessionId" cookies of
            Just sessId ->
                emit (T.MsgCookie (T.SessionCookie sessId))
            Nothing ->
                Effects.none
        )
    LoginAction a ->
        let
            loginUpdateContext = { loginAction = Just LoginSubmit, registerAction = Just RegisterSubmit }
            (loginModel, upAction, fx) = Login.update loginUpdateContext a model.loginModel
        in
            pipeMaybe
                ({ model | loginModel = loginModel }, Effects.map LoginAction fx)
                upAction
                update
    LoginSubmit username password ->
        ( { model | dbgOut = "login submit " ++ username }
        , emit (T.MsgLogin (T.SessionLogin username password))
        )
    RegisterSubmit username password ->
        ( { model | dbgOut = "register submit " ++ username }
        , emit (T.MsgRegister (T.SessionRegister username "" password))
        )

handleMessage : T.MessageFromServer -> Model -> (Model, Effects.Effects Action)
handleMessage msg model =
    case msg of
        T.MsgSession (T.SessionCookie sessId) username ->
            ( { model | sessionId = sessId, username = username, pageState = Lobby }
            , Task.onError (WebAPI.Cookie.set "sessionId" sessId) (\_ -> Task.succeed ())
                |> Task.map (always SocketSent)
                |> Effects.task
            )
        T.MsgBadCookie ->
            ( { model | dbgOut = "Bad cookie" }
            , Effects.none
            )
        T.MsgBadLogin ->
            ( { model | dbgOut = "Bad login" }
            , Effects.none
            )
        T.MsgBadRegister err ->
            ( { model | dbgOut = "Bad register: " ++ err }
            , Effects.none
            )
