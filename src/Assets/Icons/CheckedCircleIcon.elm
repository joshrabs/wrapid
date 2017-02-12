module Assets.Icons.CheckedCircleIcon exposing (..)

import Html exposing (Html, text)
import Svg exposing (svg, g, node)
import Svg.Attributes exposing (d, viewBox, id, height, width, fill, fillRule, stroke, strokeWidth, transform, points)

viewCheckedCircle: String -> String -> Html msg
viewCheckedCircle h w =
  let
    box = "0 0 " ++ h ++ " " ++ w
  in
    svg [ fill "green", height h, viewBox box, width w ]
      [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
          []
      , text "    "
      , Svg.path [ d "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm-2 15l-5-5 1.41-1.41L10 14.17l7.59-7.59L19 8l-9 9z" ]
          []
      , text ""
      ]
