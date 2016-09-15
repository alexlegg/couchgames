port module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.App
import WebSocket
import Dict
import Result
import Login
import String
import Task exposing (Task, andThen)
import Util exposing (pipeMaybe, last)
import Json.Encode exposing (encode)
import Json.Decode exposing (decodeString)
import Types as T
import Debug

wsUrl =
    "ws://localhost:8080"

main = 
    Html.App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.batch
            [ WebSocket.listen wsUrl SocketMsg
            , subCookie Cookies
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

port setCookie : String -> Cmd msg
port subCookie : (String -> msg) -> Sub msg

-- Socket handling

emit : T.MessageFromClient -> Cmd.Cmd Msg
emit msg = 
    Cmd.map (\_ -> NoOp) (WebSocket.send wsUrl (encode 0 (T.jsonEncMessageFromClient msg)))

-- Model

type PageState = Connecting | LogIn | Lobby | GameLobby | Game

type alias Model =
    { dbgOut        : String
    , pageState     : PageState
    , loginModel    : Login.Model
    , sessionId     : String
    , username      : String
    , socketConn    : Bool
    }

init : (Model, Cmd.Cmd Msg)
init = 
    ( 
        { dbgOut = ""
        , pageState = Connecting
        , loginModel = Login.init
        , sessionId = ""
        , username = ""
        , socketConn = False
        }
    , Cmd.none
    )

-- View

view : Model -> Html Msg
view model =
    case model.pageState of
        Connecting ->
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , Html.br [] []
                , text ( "Connecting to server")
                ]
        LogIn ->
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , text model.dbgOut
                , Html.br [] []
                , Html.App.map LoginMsg (Login.view model.loginModel) ]
        Lobby ->
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , text model.dbgOut
                , Html.br [] []
                , text ("Logged in as " ++ model.username)
                , button [onClick NewGame, class "purple"] [text "New Game"]
                ]
        GameLobby -> 
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , text model.dbgOut
                , Html.br [] []
                , text "Game Lobby"
                ]
        Game ->
            div [ class "container" ]
                [ Html.h1 [] [text "Couch Games"]
                , text model.dbgOut
                , Html.br [] []
                ]

-- Update

type Msg
    = SocketMsg String
    | Cookies String
    | LoginMsg Login.Msg
    | LoginSubmit String String
    | RegisterSubmit String String String
    | NewGame
    | NoOp

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update action model =
  case action of
    SocketMsg s ->
        case (decodeMessage s) of
            (Result.Ok msg) ->
                handleMessage msg model
            (Result.Err err) ->
                ( { model | dbgOut = "Error: " ++ err }, Cmd.none )
    Cookies cookies ->
        let 
            c = List.filter (String.startsWith "sessionId") (String.split ";" cookies)
        in
        case c of
            [cookie] ->
                case (last (String.split "=" cookie)) of
                    Just sessId -> 
                        ( { model | sessionId = sessId }
                        , emit (T.MsgCookie (T.SessionCookie sessId))
                        )
                    Nothing ->
                        ( { model | dbgOut = "Cookie: " ++ cookies } , Cmd.none )
            _ ->
                ( { model | dbgOut = "Cookie: " ++ cookies } , Cmd.none )
    LoginMsg a ->
        let
            loginUpdateContext = { loginMsg = Just LoginSubmit, registerMsg = Just RegisterSubmit }
            (loginModel, upMsg, fx) = Login.update loginUpdateContext a model.loginModel
        in
            pipeMaybe
                ({ model | loginModel = loginModel }, Cmd.map LoginMsg fx)
                upMsg
                update
    LoginSubmit username password ->
        ( model 
        , emit (T.MsgLogin (T.SessionLogin username password))
        )
    RegisterSubmit username password email ->
        ( model 
        , emit (T.MsgRegister (T.SessionRegister username email password))
        )
    NewGame ->
        ( { model | pageState = GameLobby }
        , emit (T.MsgNewGame model.sessionId T.Resistance)
        )
    NoOp ->
        ( model, Cmd.none )

handleMessage : T.MessageFromServer -> Model -> (Model, Cmd.Cmd Msg)
handleMessage msg model =
    case msg of
        T.MsgConnected ->
            ( { model | pageState = LogIn, socketConn = True }
            , Cmd.none
            )
        T.MsgSession (T.SessionCookie sessId) username ->
            ( { model | sessionId = sessId, username = username, pageState = Lobby }
            , setCookie sessId
            )
        T.MsgBadCookie ->
            ({ model | dbgOut = "Bad cookie" }
            , Cmd.none
            )
        T.MsgBadLogin ->
            update
                (LoginMsg (Login.ErrorMessage "Bad username or password"))
                model
        T.MsgBadRegister err ->
            update
                (LoginMsg (Login.ErrorMessage err))
                model
        T.MsgLobbyList _ ->
            ( model, Cmd.none )
