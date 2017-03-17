module Client.ExtraPortal.ExtraWardrobeStatus exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)

import Svg exposing (svg, g, node)
import Svg.Attributes exposing (d, viewBox, id, height, width, fill, fillRule, stroke, strokeWidth, transform, points)

type ExtraWardrobeStatus = NotCheckedIn | CheckedIn | CheckedOut
viewWardrobeStatus: ExtraWardrobeStatus -> Html msg
viewWardrobeStatus status =
  case status of
    NotCheckedIn ->
      div [style [("display", "flex"), ("margin", "16px")]]
      [
         div [] [wardrobeTagIcon]
        ,span [style
         [
            ("margin", "8px")
           ,("font-family", "Roboto-Regular")
           ,("font-size", "14px")
           ,("color", "#363A43")
           ,("letter-spacing", "0")
           ,("line-height", "20px")
         ]]
         [Html.text "Please checkout your clothes from wardrobe when you are ready"]
      ]
    CheckedIn -> div [] [Html.text "Checked In"]
    CheckedOut -> div [] [Html.text "Checked Out"]


wardrobeTagIcon: Html msg
wardrobeTagIcon =
  svg [ height "25px", viewBox "18 57 24 25", width "24px"]
    [
      g [ fill "none", fillRule "evenodd", id "ic_local_offer_black_24px", stroke "none", strokeWidth "1", transform "translate(18.000000, 57.000000)" ]
        [ node "polygon" [ id "Shape", points "0 0 24 0 24 24 0 24" ]
            []
        , Svg.path [ d "M21.41,11.58 L12.41,2.58 C12.05,2.22 11.55,2 11,2 L4,2 C2.9,2 2,2.9 2,4 L2,11 C2,11.55 2.22,12.05 2.59,12.42 L11.59,21.42 C11.95,21.78 12.45,22 13,22 C13.55,22 14.05,21.78 14.41,21.41 L21.41,14.41 C21.78,14.05 22,13.55 22,13 C22,12.45 21.77,11.94 21.41,11.58 Z M5.5,7 C4.67,7 4,6.33 4,5.5 C4,4.67 4.67,4 5.5,4 C6.33,4 7,4.67 7,5.5 C7,6.33 6.33,7 5.5,7 Z", fill "#50E3C2", id "Shape" ]
            []
        ]
    ]
