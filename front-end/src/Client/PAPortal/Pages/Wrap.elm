module Client.PAPortal.Pages.Wrap exposing (..)

import Html exposing (Html, div, text, span, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Common.Styles.TextStyles exposing (regularLight, regularBolder, headerTitleStyle)
import Common.Styles.ButtonStyles exposing (blackButtonStyle)

import Material.Icon as Icon exposing (view)

--MODEL
type alias Model =
  {
  }

initModel: Model
initModel = {}

type ViewState = Regular | ShowingSkin | ShowingBreakdown

--UPDATE
type Msg = OnShowSkin | OnShowBreakdown

update: Msg -> Model -> Model
update msg model =
  model

--VIEW
view: Model -> Html Msg
view model =
  div [style [
    ("display", "flex")
    ,("flex-direction", "column")
    ,("margin", "4px 8px 24px 4px")
  ]]
  [div [style [("display", "flex"), ("justify-content", "space-between")]]
    [viewWrapTimePanel
    ,viewMealPanel
    ]

  , div [] [viewReviewDayPanel]
  ]

viewWrapTimePanel: Html Msg
viewWrapTimePanel =
  div [] [text "wrap time!"]

viewMealPanel: Html Msg
viewMealPanel =
  div [] [text "Meals!"]

viewReviewDayPanel: Html Msg
viewReviewDayPanel =
  div []
  [button [] [text "Skin"]
  ,button [] [text "Breakdown"]
  ]
