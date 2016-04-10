import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import SocketIO exposing (io, emit, on)
import StartApp
import Effects
import Login
import Signal
import Task exposing (Task, andThen)
import Util exposing (pipeMaybe)
import Json.Encode exposing (encode)
import Types as T

app = 
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs =
            [ Signal.map SocketConnected connectedMB.signal
            , Signal.map SocketMsg incomingMB.signal
            ]
        }

main = 
    app.html

-- SocketIO handling

socket =
    io "http://localhost:4242" SocketIO.defaultOptions

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

{--
outgoingMB =
    Signal.mailbox "null"

port outgoing : Task x ()
port outgoing =
    socket `andThen` SocketIO.emit "MessageFromClient" incomingMB.address
--}

port emitPort : Task x ()
port emitPort =
    socket `andThen` SocketIO.emit "MessageFromClient"
        (encode 0 (T.jsonEncMessageFromClient (T.MsgCookie (T.SessionCookie "this is a cookie" "sdfdf"))))

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
    | SocketMsg String
    | SocketSent
    | LoginAction Login.Action
    | LoginSubmit String String

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    SocketConnected conn ->
        ( { model | socketConn = conn }
        , {-- emit (encode 0 (T.jsonEncMessageFromClient (T.MsgCookie (T.SessionCookie "this is a cookie"))))
            |> Task.map (always SocketSent)
            |> Effects.task --}
          Effects.none
        )
    SocketSent ->
        ( model
        , Effects.none
        )
    SocketMsg s ->
        ( { model | dbgOut = s }
        , Effects.none
        )
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
