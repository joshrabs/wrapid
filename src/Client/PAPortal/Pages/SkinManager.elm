module Client.PAPortal.Pages.SkinManager exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)

viewSkinManager: Html msg
viewSkinManager =
  div []
  [
    text "Skin will be here"
    ,viewExtrasSnapStats fakeSnapStateModel
  ]


type alias ExtrasSnapStatModel = {
  totalExtras: Int
  ,clockedIn: Int
  ,holdClothes: Int
  ,missingForms: Int
}

fakeSnapStateModel =
  { totalExtras = 100
  ,clockedIn = 90
  ,holdClothes = 20
  ,missingForms = 30
  }

viewExtrasSnapStats: ExtrasSnapStatModel -> Html msg
viewExtrasSnapStats model =
  let
      totalExtras = (model.totalExtras |> toString)

      clockRatio =
         (model.clockedIn |> toString)++ " / " ++ totalExtras

      clothingRatio =
        (model.holdClothes |> toString)++ " / " ++ totalExtras
  in

  div [style [("display", "flex")]]
  [
    div [] [text clockRatio]
    ,div [] [text clothingRatio]
    ,div [] []
  ]
