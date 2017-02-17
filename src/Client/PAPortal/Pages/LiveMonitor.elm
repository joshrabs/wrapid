module Client.PAPortal.Pages.LiveMonitor exposing (..)

import Html exposing (Html, div, text, input, button, img, span)
import Html.Attributes exposing (style, src)
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (makePanel)


type alias Model =
    { extraSnapStat : ExtrasSnapStatModel
    , table : LiveExtraTable
    }


type alias LiveExtraTable =
    List ExtraInfoItem


type alias ExtraInfoItem =
    { displayName : String
    , imgSrc : String
    , isClockedIn : Bool
    }


type alias ExtrasSnapStatModel =
    { totalExtras : Int
    , clockedIn : Int
    , holdClothes : Int
    , missingForms : Int
    }


fakeSnapStateModel : ExtrasSnapStatModel
fakeSnapStateModel =
    { totalExtras = 100
    , clockedIn = 90
    , holdClothes = 20
    , missingForms = 30
    }


fakeImg =
    "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"


fakeTable : LiveExtraTable
fakeTable =
    [ { displayName = "Steven MacCoun", imgSrc = fakeImg, isClockedIn = True }
    , { displayName = "Josh Weinberg", imgSrc = fakeImg, isClockedIn = True }
    ]


initModel : Model
initModel =
    { extraSnapStat = fakeSnapStateModel
    , table = fakeTable
    }



--VIEW


viewLiveMonitor : Model -> Html msg
viewLiveMonitor model =
    div []
        [ viewExtrasSnapStats model.extraSnapStat
        , viewLiveTable fakeTable
        ]


viewLiveTable : LiveExtraTable -> Html msg
viewLiveTable table =
    let
        panelHeader =
            Just { title = "Extras", rightItem = Nothing }

        panelBody =
            div []
                [ viewSearch
                , (viewLiveTableItems table)
                ]

        footer =
            Nothing
    in
        Dashboard.makePanel panelHeader panelBody footer


viewSearch : Html msg
viewSearch =
  div [style
    [
      ("display", "flex")
    , ("justify-content", "space-between")
    , ("align-items", "center")
    , ("height", "71px")
    , ("width", "100%")
    , ("background", "#FFFFFF")
    , ("border-bottom", "1px solid black")
    ]]
    [
      input [] []
      , div [style [
          ("display", "flex")
          ,("justify-content", "center")
          ,("align-items", "center")
          ,("background", "#FFFFFF")
          ,("box-shadow", "0 2px 4px 0 rgba(155,158,167,0.50)")
          ,("border-radius", "2px")
          ,("font-family", "Roboto-Regular")
          ,("font-size", "12px")
          ,("color", "#0000FF")
          ,("width", "72px")
          ,("height", "32px")
          ,("margin", "8px")
          ,("letter-spacing" , "0")]]
      [text "Tasks"]
    ]

viewLiveTableItems items =
    div [] (List.map (\item -> viewLiveTableItem item) items)


viewLiveTableItem : ExtraInfoItem -> Html msg
viewLiveTableItem item =
    div [ style [ ( "display", "flex" ), ( "justify-content", "space-between" ) ] ]
        [ div [ style [ ( "display", "flex" ) ] ]
            [ img
                [ src item.imgSrc
                , style
                    [ ( "border-radius", "50%" )
                    , ( "height", "36px" )
                    ]
                ]
                []
            , text item.displayName
            ]
        ]


viewExtrasSnapStats : ExtrasSnapStatModel -> Html msg
viewExtrasSnapStats model =
    let
        totalExtras =
            (model.totalExtras |> toString)

        clockRatio =
            (model.clockedIn |> toString) ++ " / " ++ totalExtras

        clothingRatio =
            (model.holdClothes |> toString) ++ " / " ++ totalExtras

        allIcons = [{num=clockRatio, text ="Clocked In"}, {num=clothingRatio, text ="Holding Clothes"}]
    in
        div [ style [ ( "display", "flex" ), ("margin", "12px 16px 12px 16px") ] ]
          (List.map
            (\r ->
              div [style [
                  ("display", "flex")
                  , ("flex-direction", "column")
                  , ("margin", "0px 12px 0px 4px")
                  , ("padding-right", "24px")
                  , ("border-right", "1px solid #EFF3F7")
                ]]
                [ span [style snapRatioStyle] [text r.num]
                , span [style snapTextStyle] [text r.text]
                ]
            )
            allIcons)


snapRatioStyle: List (String, String)
snapRatioStyle =
  [("color", "#50E3C2")
  ,("font-family", "Roboto-Regular")
  ,("font-size", "20px")
  ]

snapTextStyle: List (String, String)
snapTextStyle =
  [("color", "#282C35")
  ,("font-family", "Roboto-Regular")
  ,("font-size", "12px")
  ]

snapStatStyle: String -> List (String, String)
snapStatStyle color =
  [
    ("background-image", "radial-gradient(0% 50%, #9DFFF3 1%, #65F9DD 100%)")
    ,("box-shadow", "0 8px 30px 0 rgba(0,0,0,0.04)")
    ,("margin", "0px 8px 0px 8px")
  ]
