module Client.PAPortal.PAPortal exposing (..)

import Html exposing (..)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)

-- MODEL

type alias Model =
  { userId: Profile
  , extras: Maybe (List Profile)
  }

type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }

defaultModel: String -> Model
defaultModel id = {userId = {id=id, firstName="Nicholas", url=Nothing}, extras = Nothing}

type ViewState = ProfileWizard | FormStatus

-- UPDATE
type Msg = ChangeView ViewState


--VIEW
viewPAPortal: Model -> Html Msg
viewPAPortal model =
  div []
      [
        let
            rightItems = {avatar = Nothing}
        in
          Dashboard.view {navbar = {rightItems = Just rightItems}}
      , h1 [] [text ("Welcome to the PA portal: " ++ model.userId.firstName)]

      , case model.extras of
          Just extras ->
            ul [] (List.map viewExtras extras)
          Nothing -> div [] [text "No extras!"]
      ]


viewExtras : Profile -> Html Msg
viewExtras profile =
        li  []
            [ text profile.firstName ]
