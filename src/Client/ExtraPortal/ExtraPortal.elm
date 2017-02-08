module Client.ExtraPortal.ExtraPortal exposing (..)

import Html exposing (..)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)

-- MODEL

type alias Model = {
  profile: Profile
}

type alias Profile = {
  firstName: String
}

type ViewState = ProfileWizard | FormStatus

-- UPDATE
type Msg = ChangeView ViewState


--VIEW
viewExtraPortal: Model -> Html Msg
viewExtraPortal model =
  div []
      [
        let
            rightItems = {avatar = Nothing}
        in
          Dashboard.view {navbar = {rightItems = Just rightItems}}
      , h1 [] [text "Welcome to the extra portal!"]
      ]
