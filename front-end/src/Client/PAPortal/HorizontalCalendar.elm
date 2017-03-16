module Client.PAPortal.HorizontalCalendar exposing (..)

import Date exposing (Date, day, month)
import Date.Extra.Utils exposing (dayList)
import Date.Extra.Period exposing (add, Period(..))
import Date.Extra.Compare as DateCompare exposing (is, Compare2)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

type alias Context =
  {
     selectedDay: Date
    ,daysPrevious: Int
    ,daysNext: Int
  }

type alias HorizontalCalendar = List HorizontalCalendarItem
type alias HorizontalCalendarItem = {day: Date, isSelected: Bool}



defaultCalendar: Date -> HorizontalCalendar
defaultCalendar selectedDate =
  (dayList 20 (add Day -2 selectedDate))
    |> List.map (calendarItem selectedDate)

calendarItem: Date -> Date -> HorizontalCalendarItem
calendarItem selectedDay d =
  let
      isSelected = DateCompare.is DateCompare.Same d selectedDay
  in
    {day = d, isSelected=isSelected}

viewCalendar: (Date -> msg) -> Date -> Html msg
viewCalendar setDateMsg selectedDate =
  let
    model = defaultCalendar selectedDate
  in
    div [style [
        ("display", "flex")
        ,( "box-shadow", "inset 0 4px 8px 0 #D2D6DF" )
        ,("background", "#FFFFFF")
        ,("overflow-x", "scroll")
      ]]
    (List.map (\item -> viewCalendarItem setDateMsg item selectedDate) model)


viewCalendarItem: (Date -> msg) -> HorizontalCalendarItem -> Date -> Html msg
viewCalendarItem setDateMsg item selectedDate=
  div [onClick (setDateMsg item.day), style [
     ("display", "flex")
    ,("flex-direction", "column")
    ,("height", "120px")
    ,("min-width", "120px")
    ,("width", "120px")
    ,("font-family", "Helvetica-Light")
    ,("font-size", "7px")
    , ("opacity",
          if DateCompare.is DateCompare.After item.day selectedDate
            then "0.2" else "1.0")
  ]]
  [
    div [style [
       ("align-self", "center")
      ,("display", "flex")
      ,("flex-direction", "column")
      ,("margin", "18px 0px 2px 0px")
      ,("align-items", "center")
      ,("justify-content", "center")
    ]]
    [
        span [style [
          ("font-family", "Helvetica-Light")
          ,("font-size", "12px")
          ,("color", "#6D717A")
        ]]
        [text (item.day |> Date.dayOfWeek |> toString)]
      ,  span [style [
          ("width", "12px")
          ,("height", "2px")
          ,("margin", "4px 0px 4px 0px")
          ,("background", "#D8D8D8")
        ]][]
      , span [style [
          ("margin", "4px 0px 4px 0px")
          ,("font-family", "Helvetica-Bold")
          ,("font-size", "24px")
        ]]
        [text (day item.day |> toString)]
      , span [style [
          ("font-family", "Helvetica-Light")
          ,("font-size", "12px")
        ]]
        [text (month item.day |> toString |> String.toUpper)]
      , span [style [("color", "#FF5D00")]] [text "-"]
    ]
    ,
      if item.isSelected then
        div [style [
        ("align-self", "flex-start")
        ,("display", "flex")
        ,("align-items", "center")
        ,("margin", "auto 0px 2px 0px")
        ,("color", "#FF5D00")
      ]]
        [
          div [style
            [("width", "8px"), ("height", "8px"),("margin-right", "2px"),
              ("background", "#FF5D00")
              , ("border-radius", "100px")]] []
          ,span [style [("color", "#FF5D00")]] [text "Today"]
        ]
      else
        div [] []
  ]
