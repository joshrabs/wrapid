module Client.ExtraPortal.Pages.DailyMonitor exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (makePanel)
import Client.ExtraPortal.ExtraWardrobeStatus exposing (..)
import Client.ExtraPortal.NotificationBar as NotificationBar exposing (convertSchedule, viewNotificationBarPanel, NotificationIcon(..), NotificationBarItem)
import Client.ExtraPortal.Schedule exposing (viewSchedulePanel)

import Client.ExtraPortal.Types exposing (Schedule, TimeCard, PunchAction(..))
import Common.Styles.TextStyles exposing (headerTitleStyle)
import Date exposing (Date, day, month, year, dayOfWeek)

-- MODEL

type alias Model =
  {currentDate: Maybe Date
  ,firstName: String
  ,timecard: TimeCard
  ,schedule: Schedule
  }


--UPDATE
type Msg = TimeCardMsg PunchAction

--VIEW
viewDailyMonitor: Model -> List (Attribute Msg) -> Html Msg
viewDailyMonitor model animStyle =
  div []
      [
        viewHeader {firstName=model.firstName, production="RunabetterSet Productions", date=model.currentDate}
      , div animStyle
      [
        div [style [("margin", "8px")]]
        [
          viewNotificationBarPanel (NotificationBar.convertSchedule model.schedule)
        , Html.map TimeCardMsg (viewSchedulePanel model.schedule (Just model.timecard))
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
      ]

      ]


type alias Header = {firstName: String, production: String, date: Maybe Date}
viewHeader: Header -> Html msg
viewHeader header =
  div [style [("display", "flex"), ("flex-direction", "column"),("margin", "8px 4px 16px 16px")]]
  [
     span []
      [text
        (case header.date of
          Just date ->
            let
                toStr unit = date |> unit |> toString
            in
                (toStr dayOfWeek) ++ " " ++ (toStr month) ++ " " ++ (toStr day) ++ ", " ++ (toStr year)
          Nothing -> ""
        )
      ]
    , span [style headerTitleStyle] [text ("Welcome " ++ header.firstName)]
    ,span [] [text (header.production)]
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
viewCrewInfoItems: List CrewInfoItem -> Html msg
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


--SAMPLE DATA
-- UPDATE

defaultUrl: Maybe String
defaultUrl = Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"


defaultCrewInfoItems : List { name : String, role : String }
defaultCrewInfoItems =
  [{name = "Josh Weinberg", role="Lead PA"}
  ,{name = "Randy Lahey", role="Extra PA"}
  ,{name = "Patty Lebotomy", role="Wardrobe"}
  ]
