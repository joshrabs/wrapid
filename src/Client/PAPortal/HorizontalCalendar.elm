module Client.PAPortal.HorizontalCalendar exposing (..)

import Date exposing (Date, day, month)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)

type alias Context =
  {
     selectedDay: Date
    ,daysPrevious: Int
    ,daysNext: Int
  }

type alias HorizontalCalendar = List HorizontalCalendarItem
type alias HorizontalCalendarItem = {day: Date, isSelected: Bool}

defaultDay: Date
defaultDay =
  Date.fromString "2011/1/1" |> Result.withDefault (Date.fromTime 0)

defaultContext: Context
defaultContext =
  {
    selectedDay = defaultDay
    ,daysPrevious = 3
    ,daysNext = 3
  }


defaultCalender: HorizontalCalendar
defaultCalender = [{day = defaultDay, isSelected=False}, {day = defaultDay, isSelected=True}]

viewCalendar: Maybe HorizontalCalendar -> Html msg
viewCalendar calendar =
  let
    model =
      case calendar of
        Just calendar -> calendar
        Nothing -> defaultCalender
  in
    div [style [
      ("display", "flex")
      ,("box-shadow", "0 8px 30px 0 rgba(0,0,0,0.04)")
      ,("background", "#FFFFFF")
      ,("margin-top", "8px")
    ]]
    (List.map (\item -> viewCalendarItem item) model)


viewCalendarItem: HorizontalCalendarItem -> Html msg
viewCalendarItem item =
  div [style [
     ("display", "flex")
    ,("flex-direction", "column")
    ,("height", "120px")
    ,("width", "120px")
    ,("font-family", "Helvetica-Light")
    ,("font-size", "7px")
    -- ,("opacity", "0.2")
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
        [text "Wednesday"]
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
