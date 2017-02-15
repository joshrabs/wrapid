module Client.PAPortal.Pages.LiveMonitor exposing (..)

import Html exposing (Html, div, text, input, button, img)
import Html.Attributes exposing (style, src)

import Client.Generic.Dashboard.Dashboard as Dashboard exposing (makePanel)


type alias Model =
  {
    extraSnapStat: ExtrasSnapStatModel
    ,table: LiveExtraTable
  }

type alias LiveExtraTable = List ExtraInfoItem
type alias ExtraInfoItem =
  {
     displayName: String
    ,imgSrc: String
    ,isClockedIn: Bool
  }

type alias ExtrasSnapStatModel =
  {
    totalExtras: Int
    ,clockedIn: Int
    ,holdClothes: Int
    ,missingForms: Int
  }

fakeSnapStateModel: ExtrasSnapStatModel
fakeSnapStateModel =
  {
    totalExtras = 100
    ,clockedIn = 90
    ,holdClothes = 20
    ,missingForms = 30
  }


fakeImg = "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"

fakeTable: LiveExtraTable
fakeTable =
  [
   {displayName="Steven MacCoun", imgSrc=fakeImg, isClockedIn = True}
  ,{displayName="Josh Weinberg", imgSrc=fakeImg, isClockedIn = True}
  ]

initModel: Model
initModel =
  {
     extraSnapStat = fakeSnapStateModel
    ,table = fakeTable
  }

--VIEW


viewLiveMonitor: Model -> Html msg
viewLiveMonitor model =
  div []
  [
    viewExtrasSnapStats model.extraSnapStat
    ,viewLiveTable fakeTable
  ]


viewLiveTable: LiveExtraTable -> Html msg
viewLiveTable table =
  let
    panelHeader = Just {title ="Extras", rightItem=Nothing}
    panelBody =
      div []
      [
          viewSearch
        ,(viewLiveTableItems table)
      ]
    footer = Nothing
  in
    Dashboard.makePanel panelHeader panelBody footer


viewSearch: Html msg
viewSearch =
  input [style
    [
      ("display", "flex")
    , ("justify-content", "space-between")
    , ("align-items", "center")
    , ("height", "71px")
    , ("width", "100%")
    , ("background", "#FFFFFF")
    , ("border-bottom", "1px solid black")
    ]]
    [text "Search", button [] [text "Tasks"]]

viewLiveTableItems: List ExtraInfoItem -> Html msg
viewLiveTableItems items =
  div [] (List.map (\item -> viewLiveTableItem item) items)


viewLiveTableItem: ExtraInfoItem -> Html msg
viewLiveTableItem item =
  div [style [("display", "flex"), ("justify-content", "space-between")]]
  [
    div [style [("display", "flex")]]
    [
      img [src item.imgSrc
          , style [("border-radius", "50%"), ("height", "36px")
          ]] []
      ,text item.displayName
    ]

  ]

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
