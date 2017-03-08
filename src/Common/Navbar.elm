module Common.Navbar exposing (navbar)

import Html exposing (Html, div, img, span, text)
import Html.Attributes exposing (src, style)
import Material
import Material.Menu as Menu
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


navbar : Material.Model -> Maybe String -> List String -> Html Msg
navbar mdl avatarUrl notifications =
    let
        rightMenu =
            [ notifier mdl notifications
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
            , ( "background-color", "white" )
            , ( "width", "18px" )
            , ( "height", "3px" )
            , ( "margin", "1px 0px 2px 0px" )
            ]
        ]
        []


notifier : Material.Model -> List String -> Html Msg
notifier mdl notifications =
    let
        menuAppearance =
            [ Menu.bottomRight, Menu.ripple, Menu.icon "notifications" ]

        menuItem itm =
            Menu.Item [] [ text itm ]

        menu =
            Menu.render Mdl [ 0 ] mdl menuAppearance (List.map menuItem notifications)
    in
        div
            []
            [ div
                [ style [ ( "position", "relative" ) ] ]
                [ div
                    [ style
                        [ ( "position", "absolute" )
                        , ( "top", "-6px" )
                        , ( "right", "1px" )
                        , ( "z-index", "5" )
                        , ( "background-color", "red" )
                        , ( "color", "white" )
                        , ( "font-size", "0.8em" )
                        , ( "padding", "0px 3px 0px 3px" )
                        , ( "border-radius", "5px" )
                        , ( "border", "1px solid red" )
                        ]
                    ]
                    [ text (notifications |> List.length |> toString) ]
                ]
            , menu
            ]
