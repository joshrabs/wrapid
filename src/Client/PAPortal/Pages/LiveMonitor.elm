module Client.PAPortal.Pages.LiveMonitor exposing (..)

import Html exposing (Html, div, text, input, button, img, span)
import Html.Attributes exposing (style, src)
import Client.Generic.Dashboard.Dashboard as Dashboard exposing (makePanel)
import Assets.Icons.SearchIcon exposing (viewSearchIcon)

import Client.PAPortal.Types exposing (Profile)

type alias Model =
    { extraSnapStat : ExtrasSnapStatModel
    , table : LiveExtraTable
    }


type alias LiveExtraTable =
    List ExtraInfoItem


type alias ExtraInfoItem =
    { firstName : String
    , lastName: String
    , imgSrc : Maybe String
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
    { totalExtras = 3
    , clockedIn = 1
    , holdClothes = 2
    , missingForms = 2
    }


fakeImg =
    "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"



liveTable: Maybe (List Profile) -> LiveExtraTable
liveTable profs =
  case profs of
    Just extras ->
      (List.map (\prof ->
        {firstName=prof.firstName, lastName=prof.lastName, imgSrc=prof.avatarSrc, isClockedIn=True}
      )
      extras)
    Nothing -> []


initModel : Maybe (List Profile) -> Model
initModel profs=
    { extraSnapStat = fakeSnapStateModel
    , table = liveTable profs
    }



--VIEW


viewLiveMonitor : Model -> Html msg
viewLiveMonitor model =
    div []
        [ viewExtrasSnapStats model.extraSnapStat
        , viewLiveTable model.table
        ]


viewLiveTable : LiveExtraTable -> Html msg
viewLiveTable table =
    let
        panelHeader =
            Just { title = "Extras", rightItem = Nothing }

        panelBody =
            div []
                [ viewSearchTaskBar
                , (viewLiveTableItems table)
                ]

        footer =
            Nothing
    in
        Dashboard.makePanel panelHeader panelBody footer


viewSearchTaskBar: Html msg
viewSearchTaskBar =
  div [style
    [
      ("display", "flex")
    , ("justify-content", "space-between")
    , ("align-items", "center")
    , ("height", "71px")
    , ("width", "100%")
    , ("background", "#FFFFFF")
    , ("border-bottom", "1px solid #EFF3F7")
    ]]
    [
        viewSearch
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

viewSearch : Html msg
viewSearch =
  div [style [("margin-left", "16px"), ("position", "relative")]]
  [
    input [style [("height", "16px")]] []
    ,div [style [("position", "absolute"), ("left", "2px"), ("top", "-2px"),("display", "flex"), ("align-items", "center")]]
      [
        span [style [("margin-top", "4px")]] [viewSearchIcon]
       ,span [style
          [("font-size", "14px")
          ,("font-family", "Roboto-Regular")
          ,("color", "#45494E")
          ,("margin-left", "4px")
          ]]
         [text "Search"]
      ]
  ]

viewLiveTableItems items =
    div [] (List.map (\item -> viewLiveTableItem item) items)

getImgSrc: Maybe String -> String
getImgSrc imgSrc =
  case imgSrc of
    Just src -> src
    Nothing -> fakeImg

viewLiveTableItem : ExtraInfoItem -> Html msg
viewLiveTableItem item =
    div [ style [
          ( "display", "flex" )
          , ("align-items", "center")
          , ( "justify-content", "space-between" )
          , ( "padding", "8px 4px 8px 4px" )
        ]]
        [ div [ style [
            ( "display", "flex" )
            ,( "align-items", "center" )
            , ("margin", "8px 8px 8px 8px")
            ] ]
            [ img
                  [ src (getImgSrc item.imgSrc)
                  , style
                      [ ( "border-radius", "50%" )
                      , ( "height", "36px" )
                      ]
                  ]
                  []
            , div [style [
                ("display", "flex")
                , ("flex-direction", "column")
                , ("justify-content", "center")
                ,("margin", "4px 8px 2px 8px")
                ,("padding", "4px")
              ]]
              [
                span [style [
                  ("font-size", "14px")
                  ,("font-family", "Roboto-Regular")
                  ,("color", "#282C35")
                ]]
                [text (item.firstName ++ " " ++ item.lastName)]
                , span [style [
                    ("font-size", "12px")
                    ,("font-family", "Roboto-Regular")
                    ,("color", "#9B9EA7")
                    ,("margin", "2px 0px 2px 0px")
                  ]]
                  [text "Extra"]
              ]
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

        formRatio =
            (model.missingForms |> toString) ++ " / " ++ totalExtras

        allIcons =
          [{num=clockRatio, text ="Clocked In"}
          , {num=clothingRatio, text ="Hold Clothes"}
          , {num=formRatio, text ="Missing Forms"}
          ]
    in
        div [ style [ ( "display", "flex" ), ("justify-content", "center"), ("margin", "12px 16px 12px 16px") ] ]
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
