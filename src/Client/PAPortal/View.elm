module Client.PAPortal.View exposing (..)

import Client.Generic.Dashboard.Dashboard as Dashboard
import Client.PAPortal.Types exposing (..)
import Client.PAPortal.HorizontalCalendar exposing (viewCalendar)
import Client.PAPortal.Pages.LiveMonitor as LiveMonitor exposing (viewLiveMonitor)
import Client.PAPortal.Pages.SkinManager as Skin
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)


viewPAPortal : Model -> Html Msg
viewPAPortal model =
    div []
        [ let
            rightItems =
                { avatar = Just model.user.url }
          in
            Dashboard.view { navbar = { rightItems = Just rightItems } }
        , viewHeader model.currentView
        , viewCalendar Nothing
        , case model.currentView of
            LiveMonitor ->
                div []
                    [ viewLiveMonitor LiveMonitor.initModel
                    , button [ onClick GetAllProfiles ] [ text "Get All Profiles" ]
                    , case model.extras of
                        [] ->
                            div [] [ text "No extras!" ]

                        extras ->
                            div [] (List.map viewExtras extras)
                    ]

            SkinManager ->
                div []
                    [ Html.map SkinMsg (Skin.view model.skinModel) ]
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
