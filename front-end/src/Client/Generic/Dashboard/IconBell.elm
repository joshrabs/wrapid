module Client.Generic.Dashboard.IconBell exposing (..)
import Html exposing (Html, div)
import Html.Attributes exposing (style)

import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias IconNotification = {message: String  }
type alias IconNotifications = List IconNotification

iconBell: IconNotifications -> Html msg
iconBell notifications =
  div []
  [
      div [Html.Attributes.style [("position", "relative")]]
      [
        div [Html.Attributes.style
          [ ("position", "absolute")
            ,("top", "-9px")
            ,("right", "-7px")
            ,("background-color", "red")
            ,("color", "white")
            ,("font-size", "0.8em")
            ,("padding", "0px 3px 0px 3px")
            ,("border-radius", "5px")
            ,("border", "1px solid red")
          ]
        ][Html.text (notifications |> List.length |> toString)]
      ]
  , svg [Svg.Attributes.height "18px", viewBox "6 10 12 15", Svg.Attributes.width "16px" ]
      [Svg.path [ d "M12,24.625 C12.825,24.625 13.5,23.95 13.5,23.125 L10.5,23.125 C10.5,23.95 11.1675,24.625 12,24.625 Z M16.5,20.125 L16.5,16.375 C16.5,14.0725 15.27,12.145 13.125,11.635 L13.125,11.125 C13.125,10.5025 12.6225,10 12,10 C11.3775,10 10.875,10.5025 10.875,11.125 L10.875,11.635 C8.7225,12.145 7.5,14.065 7.5,16.375 L7.5,20.125 L6,21.625 L6,22.375 L18,22.375 L18,21.625 L16.5,20.125 Z"
        , fill "#FFFFFF"
        , id "Shape"
        , stroke "none" ]
          []
      ]
  ]


viewNotificationList: IconNotifications -> Html msg
viewNotificationList notifications =
  div [] (List.map (\notification -> div [] [text notification.message]) notifications)
