module Client.Generic.Dashboard.HamburgerMenu exposing (..)
import Html exposing (Html)

import Svg exposing (..)
import Svg.Attributes exposing (..)

menu : Html msg
menu =
    svg [ fill "#000000", attribute "height" "24", viewBox "0 0 24 24", attribute "width" "24", attribute "xmlns" "http://www.w3.org/2000/svg" ]
      [ Svg.path [ d "M0 0h24v24H0z", fill "none" ]
          []
      , text "    "
      , Svg.path [ d "M3 18h18v-2H3v2zm0-5h18v-2H3v2zm0-7v2h18V6H3z" ]
          []
      , text ""
      ]
