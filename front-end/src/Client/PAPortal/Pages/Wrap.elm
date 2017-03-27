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
    ,("flex", "1")
    ,("margin", "4px 8px 24px 4px")
  ]]
  [div [style [("display", "flex"), ("justify-content", "space-between")]]
    [viewWrapTimePanel
    ,viewMealPanel
    ]

  , viewReviewDayPanel
  ]

viewWrapTimePanel: Html Msg
viewWrapTimePanel =
  div [style panelStyle]
  [span [style panelHeaderStyle] [text "Wrap Times"]
  ,div [style orangeAddBtnStyle.container] [span [style orangeAddBtnStyle.text] [text "+ Add Wrap"]]
  ]

viewMealPanel: Html Msg
viewMealPanel =
  div [style panelStyle]
  [span [style panelHeaderStyle] [text "Meals"]
  ,div [style orangeAddBtnStyle.container] [span [style orangeAddBtnStyle.text] [text "+ Add Meal"]]
  ]

viewReviewDayPanel: Html Msg
viewReviewDayPanel =
  div [style [
    ("display", "flex")
    ,("flex-direction", "column")
    ,("justify-content", "center")
    ,("align-items", "center")
    ,("flex", "1")
    ,("background", "#23232F")
    ,("box-shadow", "2px 2px 5px 0 rgba(0,0,0,0.50)")
  ]]
  [span [style [
    ("font-family", "Roboto-Medium")
    , ("font-size", "24px")
    , ("color", "white")
    , ("margin", "16px")
    ]]
    [text "Review Day"]
  , div [style [
    ("display" , "flex")
    ,("justify-content", "space-between")
    ,("width", "70%")
    ,("margin", "8px 24px 4px 24px")
   ]]
   [ div [style bigOrangeBtnStyle] [text "Skin"]
   , div [style bigOrangeBtnStyle] [text "Breakdown"]
   ]
  ]


panelStyle =
  [("background", "#FFFFFF")
  ,("box-shadow", "2px 2px 5px 0 rgba(0,0,0,0.50)")
  ,("display", "flex")
  ,("align-items", "center")
  ,("flex-direction", "column")
  ,("min-width", "250px")
  ,("margin", "16px")
  ]

panelHeaderStyle =
  [("font-family", "Roboto-Regular")
  ,("font-size", "20px")
  ,("color", "#000000")
  ,("margin", "16px")
  ]

orangeAddBtnStyle =
  {container =
    [("background", "rgba(254,103,45,0.74)")
    ,("box-shadow", "2px 2px 5px 0 rgba(0,0,0,0.50)")
    ,("border-radius", "5px")
    ,("width", "97px")
    ]
  ,text =
    [("font-family", "Roboto-Regular")
    ,("font-size", "12px")
    ,("display", "flex")
    ,("align-items", "center")
    ,("justify-content", "center")
    ]
  }


bigOrangeBtnStyle =
  [("background", "#FE672D")
  ,("border-radius", "5px")
  ,("font-family", "Roboto-Regular")
  ,("font-size", "18px")
  ,("color", "#FFFFFF")
  ,("width", "132px")
  ,("height", "44px")
  ,("display", "flex")
  ,("align-items", "center")
  ,("justify-content", "center")
  ]
