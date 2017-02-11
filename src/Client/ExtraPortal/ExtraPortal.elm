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

defaultUrl: Maybe String
defaultUrl = Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"

defaultCrewInfoItems =
  [{name = "Josh Weinberg", role="PA"}
  ,{name = "Patty Lebotomy", role="Wardrobe"}
  ,{name = "Blah", role="Prod"}
  ]
--VIEW
viewExtraPortal: Model -> Html Msg
viewExtraPortal model =
  div []
      [
        let
            rightItems = {avatar = Just defaultUrl}
        in
          Dashboard.view {navbar = {rightItems = Just rightItems}}
      , viewHeader {firstName="Steve", production="AMC's the Walking Dead"}
      ,
        let
          panelHeader = Just {title ="Schedule"}
          panelBody = (viewSchedule ["1st task!", "2nd task!", "3rd task!"])
          footer = Nothing
        in
          Dashboard.makePanel panelHeader panelBody footer
       ,
         let
           panelHeader = Just {title ="Contact Info"}
           panelBody = (viewCrewInfoItems defaultCrewInfoItems)
           footer = Nothing
         in
           Dashboard.makePanel panelHeader panelBody footer
      ]


type alias Header = {firstName: String, production: String}
viewHeader: Header -> Html Msg
viewHeader header =
  div [style [("display", "flex"), ("flex-direction", "column"),("margin", "8px 4px 16px 16px")]]
  [
     span [] [text "Monday May 25th, 2017"]
    , span [style headerTitleStyle] [text ("Welcome " ++ header.firstName)]
    ,span [] [text (header.production)]
  ]

headerTitleStyle =
  [
  ("font-family", "Roboto-Bold")
  ,("font-size", "32px")
  ,("color", "#282C35")
  ,("letter-spacing", "0")
  ]

headerProductionStyle =
  [
  ("font-family", "Roboto-Regular")
  ,("font-size", "16px")
  ,("color", "#282C35")
  ,("letter-spacing", "0")
  ,("line-height", "20px")
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


type alias CrewInfoItem = {name: String, role: String}
viewCrewInfoItems: List CrewInfoItem -> Html Msg
viewCrewInfoItems prodContacts =
  div []
  [
    let
      listItems = List.map (\s -> (
          div [style [("display", "flex"), ("flex-direction", "column"), ("margin-left", "8px")]]
          [
            span [style [
               ("font-family", "Roboto-Medium")
              ,("font-size", "12px")
              ,("color", "#282C35")
              ,("letter-spacing", "0")
              ,("line-height", "24px")
            ]] [text s.name]
           , span [style [
              ("font-family", "Roboto")
             ,("font-size", "16px")
             ,("color", "#6D717A")
             ,("letter-spacing", "0")
             ,("line-height", "24px")
           ]][text s.role]
          ]
      )) prodContacts
    in
      div [style [("display", "flex"), ("flex-direction", "column")]] listItems
  ]
