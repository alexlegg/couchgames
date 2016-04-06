module Login where

import Html exposing (Html, form, table, tr, td, input, button, text)
import Html.Attributes exposing (type')
import Html.Events exposing (onClick, on, targetValue)
import Debug
import Effects

type alias Model =
    { username      : String
    , password      : String
    }

init : Model
init =
    Model "" ""

type Action
    = SetUsername String
    | SetPassword String
    | Submit

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        SetUsername username ->
            ({ model | username = username}, Effects.none)
        SetPassword password ->
            ({ model | password = password}, Effects.none)
        Submit ->
            ({ model | username = "", password = ""}, Effects.none)

view : Signal.Address a -> a -> Signal.Address Action -> Model -> Html
view up upAct address model =
    form []
        [ table []
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
                , td [] [button [onClick up upAct] [text "Login"]]
                ]
            ]
        ]

