module Client.ExtraPortal.NotificationBar exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)

import Svg exposing (svg, g, node)
import Svg.Attributes exposing (d, viewBox, id, height, width, fill, fillRule, stroke, strokeWidth, transform, points)

type alias NotificationBarItem = {description: String}
viewNotificationBar: List NotificationBarItem -> Html msg
viewNotificationBar items =
  div []
  [
    div []  (List.map (\item -> (viewNotificationBarItem item)) items)

  ]


viewNotificationBarItem: NotificationBarItem -> Html msg
viewNotificationBarItem item =
  div [style [("display", "inline-flex"), ("border", "1px solid black")]]
  [
      div [] [defaultTagIcon]
     ,div [] [
        span [style
         [
            ("margin", "8px")
           ,("font-family", "Roboto-Regular")
           ,("font-size", "14px")
           ,("color", "#363A43")
           ,("letter-spacing", "0")
           ,("line-height", "20px")
         ]]
         [Html.text item.description]
      ]
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
