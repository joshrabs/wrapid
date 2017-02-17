module Client.ExtraPortal.Schedule exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)

import Svg exposing (svg, g, node)
import Svg.Attributes exposing (d, viewBox, id, height, width, fill, fillRule, stroke, strokeWidth, transform, points)

import Client.ExtraPortal.Types exposing (TimeCard, Schedule)
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)

type ClockinStatus = NotClockedIn | ClockedInNotOut | ClockedOut

viewSchedulePanel: Schedule -> Maybe TimeCard -> Html msg
viewSchedulePanel schedule timecard =
  let
    panelHeader = Just {title ="Schedule", rightItem=(Just "Monday May 25, 2017")}
    panelBody = (viewSchedule schedule)
    footer = Just (
      div
        [style [("display", "flex"), ("flex-direction", "row-reverse")]]
        [
          div [style checkinButtonStyle]
          [
            span [style checkinButtonTextStyle]
            [text
              (case timecard of
                Just timecard ->
                  timecard |> clockinStatus |> clockinButtonText
                Nothing -> "Check In")

            ]
          ]
        ]
      )
  in
    Dashboard.makePanel panelHeader panelBody footer


clockinButtonText: ClockinStatus -> String
clockinButtonText status =
  case status of
    NotClockedIn -> "Check In"
    ClockedInNotOut -> "Check Out"
    ClockedOut -> "Good Work!"

clockinStatus: TimeCard -> ClockinStatus
clockinStatus timecard =
  case timecard.clockInTs of
    Just clockinTs ->
      case timecard.clockoutTs of
        Just clockoutTs -> ClockedOut
        Nothing -> ClockedInNotOut
    Nothing -> NotClockedIn

viewSchedule: Schedule -> Html msg
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

--CSS Styles
scheduleItemNameStyle : List ( String, String )
scheduleItemNameStyle =
  [
     ("font-family", "Roboto-Regular")
    ,("font-size", "12px")
    ,("color", "#363A43")
    ,("letter-spacing", "0")
  ]
scheduleItemTitleStyle : List ( String, String )
scheduleItemTitleStyle =
  [
     ("font-family", "RobotoMono-Regular")
    ,("font-size", "14px")
    ,("color", "#9B9EA7")
    ,("letter-spacing", "0")
  ]

checkinButtonStyle : List ( String, String )
checkinButtonStyle =
  [
     ("display", "flex")
    ,("justify-content", "center")
    ,("align-items", "center")
    ,("background", "#50E3C2")
    ,("box-shadow", "0 2px 2px 0 #C3C6CF")
    ,("border-radius", "2px")
    ,("border-color", "transparent")
    ,("height", "48px")
    ,("width", "114px")
    ,("margin", "8px")
  ]

checkinButtonTextStyle : List ( String, String )
checkinButtonTextStyle =
  [
     ("font-family", "Roboto-Medium")
    ,("font-size", "16px")
    ,("color", "#FFFFFF")
    ,("margin", "12px 8px 12px 8px")
  ]

defaultTagIcon: Html msg
defaultTagIcon =
  svg [ fill "#6D717A", height "24", viewBox "0 0 24 24", width "24" ]
    [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
        []
    , text "    "
    , Svg.path [ d "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z" ]
        []
    , text ""
    ]


lunchIcon: Html msg
lunchIcon =
  svg [ height "25px", viewBox "16 16 24 25", width "24px" ]
    [g [ fill "none", fillRule "evenodd", id "ic_restaurant_black_24px", stroke "none", strokeWidth "1", transform "translate(16.000000, 16.000000)" ]
        [ Svg.path [ d "M11,9 L9,9 L9,2 L7,2 L7,9 L5,9 L5,2 L3,2 L3,9 C3,11.12 4.66,12.84 6.75,12.97 L6.75,22 L9.25,22 L9.25,12.97 C11.34,12.84 13,11.12 13,9 L13,2 L11,2 L11,9 Z M16,6 L16,14 L18.5,14 L18.5,22 L21,22 L21,2 C18.24,2 16,4.24 16,6 Z", fill "#FFB62C", id "Shape" ]
            []
        , node "polygon" [ id "Shape", points "0 0 24 0 24 24 0 24" ]
            []
        ]
    ]
