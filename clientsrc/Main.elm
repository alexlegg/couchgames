import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import SocketIO exposing (io, emit, on)
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
            , Signal.map (SocketMsg << decodeString T.jsonDecMessageFromServer) incomingMB.signal
            ]
        }

main = 
    app.html

port run : Signal (Task Effects.Never ())
port run = app.tasks

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

type alias Model =
    { dbgOut        : String
    , loginModel    : Login.Model
    , socketConn    : Bool
    }

init : (Model, Effects.Effects Action)
init = 
    ( Model "" Login.init False
    , Effects.none
    )

-- View

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ text model.dbgOut
    , Html.br [] []
    , if model.socketConn then text "connected" else text "not connected"
    , Login.view (Signal.forwardTo address LoginAction) model.loginModel ]

-- Update

type Action
    = SocketConnected Bool
    | SocketMsg (Result String T.MessageFromServer)
    | SocketSent
    | LoginAction Login.Action
    | LoginSubmit String String

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    SocketConnected conn ->
        ( { model | socketConn = conn }
        , emit (T.MsgCookie (T.SessionCookie "avsdfsd" "ggssgfs"))
        )
    SocketSent ->
        ( model, Effects.none )
    SocketMsg (Result.Ok msg) ->
        handleMessage msg model
    SocketMsg (Result.Err err) ->
        ( { model | dbgOut = err }, Effects.none )
    LoginAction a ->
        let
            loginUpdateContext = { submitAction = Just LoginSubmit }
            (loginModel, upAction, fx) = Login.update loginUpdateContext a model.loginModel
        in
            pipeMaybe
                ({ model | loginModel = loginModel }, Effects.map LoginAction fx)
                upAction
                update
    LoginSubmit username password ->
           ( model
           , Effects.none
           )

handleMessage : T.MessageFromServer -> Model -> (Model, Effects.Effects Action)
handleMessage msg model =
    case msg of
        T.MsgRegistered (T.SessionCookie cookie blah) ->
            ( model
            , Effects.none
            )
