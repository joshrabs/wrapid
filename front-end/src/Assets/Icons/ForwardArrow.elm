module Assets.Icons.ForwardArrow exposing (..)

import Html exposing (Html, text)
import Svg exposing (svg, g, node)
import Svg.Attributes exposing (d, viewBox, id, height, width, fill, fillRule, stroke, strokeWidth, transform, points)

viewForwardArrow: String -> String -> Html msg
viewForwardArrow h w =
  let
    box = "0 0 " ++ h ++ " " ++ w
  in
    svg [ fill "#000000", height "24", viewBox "0 0 24 24", width "24" ]
    [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
        []
    , text "    "
    , Svg.path [ d "M12 4l-1.41 1.41L16.17 11H4v2h12.17l-5.58 5.59L12 20l8-8z" ]
        []
    , text ""
    ]
