module Login exposing
    ( init
    , update
    , view
    , Model
    , Msg(..)
    , UpdateContext
    )

import Html exposing (Html, div, table, tr, td, input, button, text)
import Html.Attributes exposing (type', value, colspan, class, placeholder)
import Html.Events exposing (onClick, onInput)
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

type Msg
    = SetUsername String
    | SetPassword String
    | SetPassword2 String
    | ErrorMessage String
    | Submit
    | GoToRegister
    | Register

type alias UpdateContext a =
    { loginMsg : Maybe (String -> String -> a)
    , registerMsg : Maybe (String -> String -> a)
    }

update : UpdateContext a -> Msg -> Model -> (Model, Maybe a, Cmd Msg)
update ctx action model =
    case action of
        SetUsername username ->
            ({ model | username = username}, Nothing, Cmd.none)
        SetPassword password ->
            ({ model | password = password}, Nothing, Cmd.none)
        SetPassword2 password ->
            ({ model | password2 = password}, Nothing, Cmd.none)
        ErrorMessage msg ->
            ({ model | errorMessage = Just msg}, Nothing, Cmd.none)
        Submit ->
            ( { model | password = "", password2 = ""}
            , applyHandler2 ctx.loginMsg model.username model.password
            , Cmd.none
            )
        GoToRegister ->
            ({ model | formState = Registering, errorMessage = Nothing }, Nothing, Cmd.none)
        Register ->
            if model.password == model.password2
            then
                ( { model | password = "", password2 = "" }
                , applyHandler2 ctx.registerMsg model.username model.password
                , Cmd.none
                )
            else
                ( { model | errorMessage = Just "Passwords don't match", password = "", password2 = "" }
                , Nothing
                , Cmd.none
                )

view : Model -> Html Msg
view model =
    div [class "card"]
        ( viewErrorMessage model
        ++ [ input
            [ type' "text"
            , onInput SetUsername
            , placeholder "Username"
            , class "login-input"
            , value model.username
            ]
            []
        , input
            [ type' "password"
            , onInput SetPassword
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
                    , onInput SetPassword2
                    , placeholder "Confirm Password"
                    , class "login-input"
                    , value model.password2
                    ]
                    []
                , button [onClick Register , class "light-purple"] [text "Register"]
                ]
            else
                [ button [onClick Submit, class "purple"] [text "Login"]
                , button [onClick GoToRegister, class "light-purple"] [text "I need an Account"]
                ]
        )

viewErrorMessage model =
    case model.errorMessage of
        Just msg ->
            [ text msg ]
        Nothing ->
            []
