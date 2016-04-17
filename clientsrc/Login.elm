module Login
    ( init
    , update
    , view
    , Model
    , Action(..)
    , UpdateContext
    ) where

import Html exposing (Html, div, table, tr, td, input, button, text)
import Html.Attributes exposing (type', value, colspan, class, placeholder)
import Html.Events exposing (onClick, on, targetValue)
import Effects exposing (Effects)
import Util exposing (applyHandler2)

type FormState = LoggingIn | Registering

type alias Model =
    { formState     : FormState
    , errorMessage  : Maybe String
    , username      : String
    , password      : String
    , password2     : String
    }

init : Model
init =
    { formState = LoggingIn
    , errorMessage = Nothing
    , username = ""
    , password = ""
    , password2 = ""
    }

type Action
    = SetUsername String
    | SetPassword String
    | SetPassword2 String
    | ErrorMessage String
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
        ErrorMessage msg ->
            ({ model | errorMessage = Just msg}, Nothing, Effects.none)
        Submit ->
            ( { model | password = "", password2 = ""}
            , applyHandler2 ctx.loginAction model.username model.password
            , Effects.none
            )
        GoToRegister ->
            ({ model | formState = Registering, errorMessage = Nothing }, Nothing, Effects.none)
        Register ->
            if model.password == model.password2
            then
                ( { model | password = "", password2 = "" }
                , applyHandler2 ctx.registerAction model.username model.password
                , Effects.none
                )
            else
                ( { model | errorMessage = Just "Passwords don't match", password = "", password2 = "" }
                , Nothing
                , Effects.none
                )

view : Signal.Address Action -> Model -> Html
view address model =
    div [class "card"]
        ( viewErrorMessage model
        ++ [ input
            [ type' "text"
            , on "input" targetValue (Signal.message address << SetUsername)
            , placeholder "Username"
            , class "login-input"
            , value model.username
            ]
            []
        , input
            [ type' "password"
            , on "input" targetValue (Signal.message address << SetPassword)
            , placeholder "Password"
            , class "login-input"
            , value model.password
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
                    , value model.password2
                    ]
                    []
                , button [onClick address Register , class "light-purple"] [text "Register"]
                ]
            else
                [ button [onClick address Submit, class "purple"] [text "Login"]
                , button [onClick address GoToRegister, class "light-purple"] [text "I need an Account"]
                ]
        )

viewErrorMessage model =
    case model.errorMessage of
        Just msg ->
            [ text msg ]
        Nothing ->
            []
