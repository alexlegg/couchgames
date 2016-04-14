module Login
    ( init
    , update
    , view
    , Model
    , Action
    , UpdateContext
    ) where

import Html exposing (Html, div, table, tr, td, input, button, text)
import Html.Attributes exposing (type', colspan, class, placeholder)
import Html.Events exposing (onClick, on, targetValue)
import Effects exposing (Effects)
import Util exposing (applyHandler2)

type FormState = LoggingIn | Registering

type alias Model =
    { formState     : FormState
    , username      : String
    , password      : String
    , password2     : String
    }

init : Model
init =
    { formState = LoggingIn
    , username = ""
    , password = ""
    , password2 = ""
    }

type Action
    = SetUsername String
    | SetPassword String
    | SetPassword2 String
    | Submit
    | GoToRegister
    | Register

type alias UpdateContext a =
    { loginAction : Maybe (String -> String -> a)
    , registerAction : Maybe (String -> String -> a)
    }

update : UpdateContext a -> Action -> Model -> (Model, Maybe a, Effects Action)
update ctx action model =
    case action of
        SetUsername username ->
            ({ model | username = username}, Nothing, Effects.none)
        SetPassword password ->
            ({ model | password = password}, Nothing, Effects.none)
        SetPassword2 password ->
            ({ model | password2 = password}, Nothing, Effects.none)
        Submit ->
            ( { model | username = "", password = "", password2 = ""}
            , applyHandler2 ctx.loginAction model.username model.password
            , Effects.none
            )
        GoToRegister ->
            ({ model | formState = Registering }, Nothing, Effects.none)
        Register ->
            if model.password == model.password2
            then
                ( { model | username = "", password = "", password2 = "" }
                , applyHandler2 ctx.registerAction model.username model.password
                , Effects.none
                )
            else
                ( { model | username = "", password = "", password2 = "" }
                , Nothing
                , Effects.none
                )

view : Signal.Address Action -> Model -> Html
view address model =
    div [class "card"]
        ([ input
            [ type' "text"
            , on "input" targetValue (Signal.message address << SetUsername)
            , placeholder "Username"
            , class "login-input"
            ]
            []
        , Html.br [] []
        , input
            [ type' "password"
            , on "input" targetValue (Signal.message address << SetPassword)
            , placeholder "Password"
            , class "login-input"
            ]
            []
        ] 
        ++ if model.formState == Registering
            then
                [ input
                    [ type' "password"
                    , on "input" targetValue (Signal.message address << SetPassword2)
                    , placeholder "Confirm Password"
                    , class "login-input"
                    ]
                    []
                , button [onClick address Register , class "light-purple"] [text "Register"]
                ]
            else
                [ button [onClick address Submit, class "purple"] [text "Login"]
                , button [onClick address GoToRegister, class "light-purple"] [text "I need an Account"]
                ]
        )
