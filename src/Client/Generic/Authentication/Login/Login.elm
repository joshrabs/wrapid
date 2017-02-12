module Client.Generic.Authentication.Login.Login exposing (loginView, Msg, initModel)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Material.Textfield as Textfield
import Material
import Material.Scheme

--MODEL

type alias Model = {
  email: Maybe String,
  password: Maybe String
  ,mdl : Material.Model
}

initModel: Maybe String -> Maybe String -> Model
initModel email password =
  {
    email = email
    ,password = password
    ,mdl = Material.model
  }
--UPDATE
type Msg =
   Email String
  | Password String
  | SubmitLogin
  | Mdl (Material.Msg Msg)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Email email -> ({model | email = Just email}, Cmd.none)
    Password password -> ({model | password = Just password}, Cmd.none)
    SubmitLogin -> (model, Cmd.none)
    Mdl msg_ ->
      Material.update Mdl msg_ model

--VIEW

type alias Mdl =
    Material.Model

loginView: Model -> Html Msg
loginView model =
      div []
      [ input [ type_ "text", placeholder "Name", onInput Email ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , Textfield.render Mdl
            [10,0]
            model.mdl
            [ Textfield.label "Enter email"
            , Textfield.floatingLabel
            , Textfield.email

            ]
            []
        , Textfield.render Mdl
            [10,1]
            model.mdl
            [ Textfield.label "Enter password"
            , Textfield.floatingLabel
            , Textfield.password

            ]
            []
      ]
      |> Material.Scheme.top
