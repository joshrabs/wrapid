module Client.ExtraPortal.ExtraPortal exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)
import Client.ExtraPortal.ExtraWardrobeStatus exposing (..)
import Client.ExtraPortal.NotificationBar exposing (..)

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

defaultNotificationItems =
  [
    {description="Lunch in 1 Hour", icon=LunchIcon, startTm="12:00 PM", endTm="1:00 PM"}
    ,{description="Shoot Zombie Set", icon=Default, startTm="4:00 PM", endTm="4:30 PM"}
  ]

defaultCrewInfoItems =
  [{name = "Josh Weinberg", role="Lead PA"}
  ,{name = "Randy Lahey", role="Extra PA"}
  ,{name = "Patty Lebotomy", role="Wardrobe"}
  ]

defaultScheduleItems =
  [
    {name="Start Time", startTm="8:00 AM"}
    ,{name="Break for Lunch", startTm="12:00 PM"}
    ,{name="Estimated End Time", startTm="6:00 PM"}
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
      , viewNotificationBarPanel defaultNotificationItems
      ,
        let
          panelHeader = Just {title ="Schedule", rightItem=(Just "Monday May 25, 2017")}
          panelBody = (viewSchedule defaultScheduleItems)
          footer = Nothing
        in
          Dashboard.makePanel panelHeader panelBody footer
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

type alias Schedule = List ScheduleItem
type alias ScheduleItem = {name: String, startTm: String}
viewSchedule: Schedule -> Html Msg
viewSchedule schedule =
  div []
  [
    let
      listItems = List.map (\s -> (
          div [style [("display", "flex"), ("justify-content", "space-between"), ("margin", "8px")]]
            [
              span [style scheduleItemNameStyle] [text s.name]
              ,span [style scheduleItemTitleStyle] [text s.startTm]
            ]
      )) schedule
    in
      div [] listItems
  ]

scheduleItemNameStyle =
  [
     ("font-family", "Roboto-Regular")
    ,("font-size", "12px")
    ,("color", "#363A43")
    ,("letter-spacing", "0")
  ]

scheduleItemTitleStyle =
  [
     ("font-family", "RobotoMono-Regular")
    ,("font-size", "14px")
    ,("color", "#9B9EA7")
    ,("letter-spacing", "0")
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
