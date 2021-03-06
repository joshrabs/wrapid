module Client.ExtraPortal.Pages.FormStatusPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, src, href)
import Html.Events exposing (onClick)
import Assets.Icons.CheckedCircleIcon exposing (viewCheckedCircle)
import Assets.Icons.ForwardArrow exposing (viewForwardArrow)
import Animation exposing (px)


defaultImgSrc =
    "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciz8vkq0f0jdc0168aoxv5h3w"


type alias Model =
    FormStatuses


type alias FormStatuses =
    List FormStatus


type alias FormStatus =
    { id : Maybe Int
    , formName : String
    , url : String
    , completedDt : String
    , completedTs : String
    , imgSrc : String
    }


viewFormStatusPage : msg -> FormStatuses -> List (Attribute msg) -> Html msg
viewFormStatusPage msg statuses animStyle =
    div [ style [ ( "margin", "8px" ) ] ]
        [ div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                ]
            ]
            [ span
                [ style
                    [ ( "font-family", "Roboto-Bold" )
                    , ( "font-size", "32px" )
                    , ( "color", "#282C35" )
                    , ( "margin-top", "8px" )
                    , ( "letter-spacing", "0" )
                    ]
                ]
                [ text "Your Completed Forms" ]
            ]
        , span
            [ onClick msg
            , style
                [ ( "display", "flex" )
                , ( "align-items", "center" )
                , ( "font-family", "Roboto" )
                , ( "font-size", "16px" )
                , ( "letter-spacing", "0" )
                , ( "line-height", "20px" )
                , ( "margin-top", "8px" )
                , ( "margin", "8px 0px 4px 0px" )
                ]
            ]
            [ text "Go to daily tasks", viewForwardArrow "24" "24" ]
        , div animStyle (List.map (\item -> viewFormStatusIcon item) statuses)
        ]


viewFormStatusIcon : FormStatus -> Html msg
viewFormStatusIcon item =
    let
        link =
            case item.id of
                Just id ->
                    [ href ("http://35.157.165.22/profiles/" ++ (toString id) ++ "/" ++ item.url) ]

                _ ->
                    []
    in
        a link
            [ div
                [ style
                    [ ( "display", "inline-flex" )
                    , ( "flex-direction", "column" )
                    , ( "background", "#FFFFFF" )
                    , ( "box-shadow", "0 8px 8px 0 rgba(210,214,223,0.70)" )
                    , ( "margin", "8px 16px 8px 0px" )
                    ]
                ]
                [ img
                    [ src defaultImgSrc
                    , style [ ( "width", "165px" ), ( "height", "118px" ) ]
                    ]
                    []
                , div
                    [ style
                        [ ( "display", "flex" )
                        , ( "flex-direction", "column" )
                        , ( "margin", "8px" )
                        ]
                    ]
                    (completedStatus item)
                ]
            ]


completedStatus : FormStatus -> List (Html msg)
completedStatus item =
    case item.id of
        Just id ->
            [ span [ style formNameCss ] [ text item.formName ]
            , div [ style [ ( "display", "flex" ), ( "align-items", "center" ), ( "margin", "2px 0px 2px 0px" ) ] ]
                [ div [ style [ ( "width", "24px" ), ( "height", "24px" ) ] ] [ viewCheckedCircle "24" "24" ]
                , span [ style completedDtCss ] [ text ("Completed. " ++ item.completedDt) ]
                ]
            , span [ style completedDtCss ] [ text item.completedTs ]
            ]

        Nothing ->
            [ span [ style formNameCss ] [ text item.formName ]
            , div [ style [ ( "display", "flex" ), ( "align-items", "center" ), ( "margin", "2px 0px 2px 0px" ) ] ]
                [ span [ style completedDtCss ] [ text ("Incompleted.") ]
                ]
            ]


formNameCss =
    [ ( "font-family", "Roboto-Regular" )
    , ( "font-size", "14px" )
    , ( "margin-bottom", "2px" )
    ]


completedDtCss =
    [ ( "font-family", "Roboto-Regular" )
    , ( "font-size", "12px" )
    , ( "color", "#9B9EA7" )
    , ( "letter-spacing", "0" )
    , ( "margin-left", "2px" )
    ]
