module Client.Generic.Status.Loading exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

import Material.Spinner as Loading

viewLoadingScreen: Html msg
viewLoadingScreen =
  div [style [
     ("display", "flex")
    ,("justify-content", "center")
    ,("align-items", "center")
    ,("height", "100%")
  ]]
  [
    Loading.spinner
      [ Loading.active True ]
  ]
