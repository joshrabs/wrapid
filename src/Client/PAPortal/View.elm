module Client.PAPortal.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)

import Client.PAPortal.Types exposing (..)
import Client.PAPortal.HorizontalCalendar exposing (viewCalendar, defaultCalendar)
import Client.PAPortal.Pages.LiveMonitor as LiveMonitor exposing (viewLiveMonitor)
import Client.PAPortal.Pages.SkinManager as Skin
import Client.PAPortal.Pages.Wrap as Wrap exposing (viewWrap)
import Client.Generic.Status.Loading exposing (viewLoadingScreen)

viewPAPortal : Model -> Html Msg
viewPAPortal model =
    div []
        [ viewHeader model.currentView
        ,  case model.selectedDate of
            Just selectedDate -> viewCalendar SetSelectedDate selectedDate
            Nothing -> div [] []
        , div [style [("margin-top", "16px"), ("overflow-y", "scroll")]]
          [case model.currentView of
            LiveMonitor ->
              case model.extras of
                Loading -> viewLoadingScreen
                Success extras ->
                  div []
                      [ viewLiveMonitor (LiveMonitor.initModel extras model.mdl)
                      ]

            SkinManager ->
                div []
                    [ Html.map SkinMsg (Skin.view model.skinModel) ]
            Wrap ->
              div []
                [Html.map WrapMsg (Wrap.viewWrap model.wrapModel) ]
          ]
        ]


viewHeader : ViewState -> Html Msg
viewHeader currentView =
    let
        getTabStyle tabType =
            if currentView == tabType then
                selectedTabStyle
            else
                baseTabStyle
    in
        div [ style [ ( "background-color", "#FFFFFF" ) ] ]
            [ viewHeaderInfo
            , div [ style [ ( "display", "flex" ), ( "border-top", "1px solid #EBF0F5" ) ] ]
                [ div
                    [ onClick (ChangeView LiveMonitor)
                    , style (getTabStyle LiveMonitor)
                    ]
                    [ text "Live Monitor" ]
                , div
                    [ onClick (ChangeView SkinManager)
                    , style (getTabStyle SkinManager)
                    ]
                    [ text "Skin Manager" ]
                , div
                    [ onClick (ChangeView Wrap)
                    , style (getTabStyle Wrap)
                    ]
                    [ text "Wrap" ]
                ]
            ]


baseTabStyle : List ( String, String )
baseTabStyle =
    [ ( "padding", "8px" )
    , ( "font-size", "14px" )
    , ( "font-family", "Roboto-Regular" )
    , ( "font-family", "Roboto-Regular" )
    , ( "color", "#6D717A" )
    ]


selectedTabStyle : List ( String, String )
selectedTabStyle =
    List.concat
        [ baseTabStyle
        , [ ( "border-bottom", "2px solid black" )
          , ( "color", "black" )
          , ( "font-family", "Roboto-Medium" )
          ]
        ]


viewHeaderInfo : Html msg
viewHeaderInfo =
    div [ style [] ]
        [ div
            [ style
                [ ( "margin", "16px" )
                , ( "display", "inline-flex" )
                , ( "flex-direction", "column" )
                ]
            ]
            [ span
                [ style
                    [ ( "font-family", "Roboto-Regular" )
                    , ( "font-size", "12px" )
                    , ( "color", "#6D717A" )
                    ]
                ]
                [ text "Monday May 25, 2016" ]
            , span
                [ style
                    [ ( "font-family", "Roboto-Regular" )
                    , ( "font-size", "16px" )
                    , ( "margin", "8px 0px 8px 0px" )
                    , ( "color", "#282C35" )
                    , ( "letter-spacing", "0" )
                    ]
                ]
                [ text "RUNABETTERSET Productions" ]
            ]
        ]


viewExtras : Profile -> Html Msg
viewExtras profile =
    li []
        [ text profile.firstName ]
