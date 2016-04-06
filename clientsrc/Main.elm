import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import StartApp
import Effects
import Login

app = 
    StartApp.start { init = init, update = update, view = view, inputs = [] }

main = 
    app.html

type alias Model =
    { loginModel    : Login.Model
    }

init : (Model, Effects.Effects Action)
init = (Model Login.init, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ Login.view address LoginSubmit (Signal.forwardTo address LoginAction) model.loginModel ]

type Action
    = LoginAction Login.Action
    | LoginSubmit

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  case action of
    LoginAction a ->
        let
            (loginModel, fx) = Login.update a model.loginModel
        in
            ( { model | loginModel = loginModel }
            , Effects.map LoginAction fx
            )
    LoginSubmit ->
           ( model
           , Effects.none
           )

