module Client.Generic.Authentication.Login.Login exposing (loginView, Msg)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

--MODEL

type alias Model = {
  email: String,
  password: String
}

--UPDATE
type Msg = Email String | Password String | SubmitLogin

update: Msg -> Model -> Model
update msg model =
  case msg of
    Email email -> {model | email = email}
    Password password -> {model | password = password}
    SubmitLogin -> model

--VIEW

loginView: Maybe Model -> Html Msg
loginView model =
  case model of
    Just model ->
      div []
      [ input [ type_ "text", placeholder "Name", onInput Email ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
      ]
    Nothing ->
      div []
      [ input [ type_ "text", placeholder "Name", onInput Email ] []
        , input [ type_ "password", placeholder "Password", onInput Password ] []
        , button [ onClick SubmitLogin ] [ text "Login" ]
      ]
