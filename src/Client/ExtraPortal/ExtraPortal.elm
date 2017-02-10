module Client.ExtraPortal.ExtraPortal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

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
      ,
        let
          panelHeader = Just {title ="Schedule"}
          panelBody = (viewSchedule ["1st task!", "2nd task!", "3rd task!"])
          footer = Nothing
        in
          Dashboard.makePanel panelHeader panelBody footer
      ]



type alias Schedule = List String
viewSchedule: Schedule -> Html Msg
viewSchedule schedule =
  div []
  [
    let
      listItems = List.map (\s -> (div [] [text s])) schedule
    in
      div [] listItems
  ]
