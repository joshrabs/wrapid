module Common.Navbar exposing (navbar)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Material
import Material.Menu as Menu
import Material.Icon as Icon
import Svg exposing (svg)
import Svg.Attributes exposing (d, fill, height, id, path, stroke, viewBox, width)
import Common.Styles as Styles
import Common.WrapidLogo exposing (logo)
import Types exposing (Msg(..))


type alias Config msg =
    { toggleMenuMsg : msg
    , selectItemMsg : String -> msg
    }


type alias Context =
    { isOpen : Bool
    }


navbar : Material.Model -> Config Msg -> Context -> Maybe String -> List String -> Html Msg
navbar mdl config context avatarUrl notifications =
    let
        rightMenu =
            [ notifier mdl config context notifications
            , avatar avatarUrl
            , hamburgerMenu
            ]
    in
        div [ style Styles.navContainer ]
            [ div
                [ style [ ( "margin-left", "16px" ) ] ]
                [ logo ]
            , div
                [ style Styles.navbarRight ]
                (List.map rightItemBox rightMenu)
            ]


rightItemBox : Html msg -> Html msg
rightItemBox htmlMsg =
    div [ style [ ( "padding", "0px 8px 0px 8px" ) ] ] [ htmlMsg ]


avatar : Maybe String -> Html msg
avatar mStr =
    let
        defaultAvatarUrl =
            "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"

        url =
            Maybe.withDefault defaultAvatarUrl mStr
    in
        img [ style [ ( "width", "40px" ), ( "border-radius", "50%" ) ], src url ] []


hamburgerMenu : Html msg
hamburgerMenu =
    div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
        [ hamburgerBarItem
        , hamburgerBarItem
        , hamburgerBarItem
        ]


hamburgerBarItem : Html msg
hamburgerBarItem =
    span
        [ style
            [ ( "border-radius", "1px" )
            , ( "background-color", "black" )
            , ( "width", "18px" )
            , ( "height", "3px" )
            , ( "margin", "1px 0px 2px 0px" )
            ]
        ]
        []


notifier : Material.Model -> Config Msg -> Context -> List String -> Html Msg
notifier mdl config context notifications =
    let
        bell =
            Icon.view "notifications" []

        menuAppearance =
            [ Menu.bottomRight, Menu.ripple, Menu.icon "notifications" ]

        menuItem itm =
            Menu.Item [] [ text itm ]

        menu =
            Menu.render Mdl [ 0 ] mdl menuAppearance (List.map menuItem notifications)
    in
        -- div [ onClick config.toggleMenuMsg ]
        --     [ div [ Html.Attributes.style [ ( "position", "relative" ) ] ]
        --         [ div
        --             [ Html.Attributes.style
        --                 [ ( "position", "absolute" )
        --                 , ( "top", "-9px" )
        --                 , ( "right", "-7px" )
        --                 , ( "background-color", "red" )
        --                 , ( "color", "white" )
        --                 , ( "font-size", "0.8em" )
        --                 , ( "padding", "0px 3px 0px 3px" )
        --                 , ( "border-radius", "5px" )
        --                 , ( "border", "1px solid red" )
        --                 ]
        --             ]
        --             [ Html.text (notifications |> List.length |> toString) ]
        --         ]
        --     , svg [ height "18px", viewBox "6 10 12 15", width "16px" ]
        --         [ Svg.path
        --             [ d "M12,24.625 C12.825,24.625 13.5,23.95 13.5,23.125 L10.5,23.125 C10.5,23.95 11.1675,24.625 12,24.625 Z M16.5,20.125 L16.5,16.375 C16.5,14.0725 15.27,12.145 13.125,11.635 L13.125,11.125 C13.125,10.5025 12.6225,10 12,10 C11.3775,10 10.875,10.5025 10.875,11.125 L10.875,11.635 C8.7225,12.145 7.5,14.065 7.5,16.375 L7.5,20.125 L6,21.625 L6,22.375 L18,22.375 L18,21.625 L16.5,20.125 Z"
        --             , fill "#000000"
        --             , id "Shape"
        --             , stroke "none"
        --             ]
        --             []
        --         ]
        --     , menu
        --     ]
        div [] [ menu ]
