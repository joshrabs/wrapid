module Client.ExtraPortal.Pages.DailyMonitor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)
import Client.ExtraPortal.ExtraWardrobeStatus exposing (..)
import Client.ExtraPortal.NotificationBar exposing (..)
import Client.ExtraPortal.Schedule exposing (..)

-- MODEL

type alias Model = Maybe String

type alias Profile = {
  firstName: String
}

type ViewState = ProfileWizard | FormStatus | DailyMonitor

-- UPDATE
type Msg = ChangeView ViewState

defaultUrl: Maybe String
defaultUrl = Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"

defaultNotificationItems: List NotificationBarItem
defaultNotificationItems =
  [
    {description="Lunch in 1 Hour", icon=LunchIcon, startTm="12:00 PM", endTm="1:00 PM"}
    ,{description="Shoot Zombie Set", icon=Default, startTm="4:00 PM", endTm="4:30 PM"}
  ]

defaultCrewInfoItems : List { name : String, role : String }
defaultCrewInfoItems =
  [{name = "Josh Weinberg", role="Lead PA"}
  ,{name = "Randy Lahey", role="Extra PA"}
  ,{name = "Patty Lebotomy", role="Wardrobe"}
  ]

defaultScheduleItems : Schedule
defaultScheduleItems =
  [
    {name="Start Time", startTm="8:00 AM"}
    ,{name="Break for Lunch", startTm="12:00 PM"}
    ,{name="Estimated End Time", startTm="6:00 PM"}
  ]
--VIEW
viewDailyMonitor: Model -> Html Msg
viewDailyMonitor model =
  div []
      [
        let
            rightItems = {avatar = Just defaultUrl}
        in
          Dashboard.view {navbar = {rightItems = Just rightItems}}
      , viewHeader {firstName="Steve", production="AMC's the Walking Dead"}
      , viewNotificationBarPanel defaultNotificationItems
      , viewSchedulePanel defaultScheduleItems
       ,
         let
           panelHeader = Just {title ="Wardrobe", rightItem=Nothing}
           panelBody = (viewWardrobeStatus NotCheckedIn)
           footer = Nothing
         in
           Dashboard.makePanel panelHeader panelBody footer
      ,
         let
           panelHeader = Just {title ="Contact Info", rightItem=Nothing}
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

headerTitleStyle : List ( String, String )
headerTitleStyle =
  [
  ("font-family", "Roboto-Bold")
  ,("font-size", "32px")
  ,("color", "#282C35")
  ,("letter-spacing", "0")
  ]

headerProductionStyle : List ( String, String )
headerProductionStyle =
  [
  ("font-family", "Roboto-Regular")
  ,("font-size", "16px")
  ,("color", "#282C35")
  ,("letter-spacing", "0")
  ,("line-height", "20px")
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
