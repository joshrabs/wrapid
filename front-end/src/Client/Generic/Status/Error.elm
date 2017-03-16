module Client.Generic.Status.Loading exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

import Material.Spinner as Loading

viewErrorScreen: Maybe ErrorMsg -> Html msg
viewErrorScreen errorMsg =
  div [style [
     ("display", "flex")
    ,("justify-content", "center")
    ,("align-items", "center")
    ,("height", "100%")
  ]]
  [

        h1 []
        [case errorMsg of
          Nothing ->
            text "Looks like something went wrong! We are working on it"]
          Just errorMsg ->
            text errorMsg
        ]
  ]
