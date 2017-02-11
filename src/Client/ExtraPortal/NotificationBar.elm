module Client.ExtraPortal.NotificationBar exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)

import Svg exposing (svg, g, node)
import Svg.Attributes exposing (d, viewBox, id, height, width, fill, fillRule, stroke, strokeWidth, transform, points)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (..)


type alias NotificationBarItem =
  {description: String
  , icon: NotificationIcon
  , startTm: String
  , endTm: String
  }

type NotificationIcon = Default | LunchIcon

viewNotificationBarPanel: List NotificationBarItem -> Html msg
viewNotificationBarPanel items =
  let
    panelHeader = Just {title ="Notifications", rightItem=(Just (items |> List.length |> toString))}
    panelBody = viewNotificationBar items
    footer = Nothing
  in
    Dashboard.makePanel panelHeader panelBody footer


viewNotificationBar: List NotificationBarItem -> Html msg
viewNotificationBar items =
  div
  [style
    [
      ("display", "flex")
    ]
  ]
  (List.map (\item -> (viewNotificationBarItem item)) items)


viewNotificationBarItem: NotificationBarItem -> Html msg
viewNotificationBarItem item =
  div [style
   [
     ("display", "flex")
    ,("margin", "8px")
    ,("background", "#FFFFFF")
    ,("box-shadow", "0 2px 4px 0 #D2D6DF")
    ,("height", "78px")
    ,("width", "259px")
    ]
  ]
  [
      div []
      [
        case item.icon of
          LunchIcon -> lunchIcon
          Default -> defaultTagIcon
      ]
     ,div []
       [
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
        ,
          span [style
           [
              ("margin", "8px")
             ,("font-family", "RobotoMono-Regular")
             ,("font-size", "14px")
             ,("color", "#6D717A")
             ,("letter-spacing", "0")
             ,("line-height", "20px")
           ]]
           [Html.text (item.startTm ++ " - " ++ item.endTm)]
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
