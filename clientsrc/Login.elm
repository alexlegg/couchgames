module Login
    ( init
    , update
    , view
    , Model
    , Action
    , UpdateContext
    ) where

import Html exposing (Html, form, table, tr, td, input, button, text)
import Html.Attributes exposing (type')
import Html.Events exposing (onClick, on, targetValue)
import Effects exposing (Effects)
import Util exposing (applyHandler2)

type alias Model =
    { username      : String
    , password      : String
    }

init : Model
init =
    { username = ""
    , password = ""
    }

type Action
    = SetUsername String
    | SetPassword String
    | Submit

type alias UpdateContext a =
    { submitAction  : Maybe (String -> String -> a)
    }

update : UpdateContext a -> Action -> Model -> (Model, Maybe a, Effects Action)
update ctx action model =
    case action of
        SetUsername username ->
            ({ model | username = username}, Nothing, Effects.none)
        SetPassword password ->
            ({ model | password = password}, Nothing, Effects.none)
        Submit ->
            ( { model | username = "", password = ""}
            , applyHandler2 ctx.submitAction model.username model.password
            , Effects.none
            )

view : Signal.Address Action -> Model -> Html
view address model =
    table []
        [ tr []
            [ td [] [text "Username"]
            , td [] [input [type' "text", on "input" targetValue (Signal.message address << SetUsername)] []]
            ]
        , tr []
            [ td [] [text "Password"]
            , td [] [input [type' "password", on "input" targetValue (Signal.message address << SetPassword)] []]
            ]
        , tr []
            [ td [] []
            , td [] [button [onClick address Submit] [text "Login"]]
            ]
        ]

