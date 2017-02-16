module Client.Generic.Dashboard.Dashboard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Client.Generic.Dashboard.IconBell exposing (iconBell)
import Client.Generic.Dashboard.WrapidLogo exposing (logo)


--MODEL


type alias Model =
    { navbar : NavbarModel
    }


type alias NavbarModel =
    { rightItems : Maybe RightItems
    }


type alias RightItems =
    { avatar : Maybe Avatar
    }


type alias Avatar =
    Maybe String


type alias NotificationBell =
    List Notification


type alias Notification =
    String



--UPDATE


type Msg
    = NoOp



-- VIEW


view : Model -> Html msg
view model =
    div []
        [ navbarView model.navbar
        ]


type alias PanelHeader =
    { title : String, rightItem : Maybe String }


type alias PanelFooter footerMsg =
    Html footerMsg


makePanel : Maybe PanelHeader -> Html msg -> Maybe (PanelFooter msg) -> Html msg
makePanel header body footer =
    let
        panelHeader =
            case header of
                Just header ->
                    div [ style panelHeaderStyle ]
                        [ span [ style panelHeaderItemStyle ] [ text header.title ]
                        , case header.rightItem of
                            Just rightItem ->
                                span [ style panelHeaderItemStyle ] [ text rightItem ]

                            Nothing ->
                                span [] []
                        ]

                Nothing ->
                    div [] []

        panelFooter =
            case footer of
                Just footer ->
                    div [ style panelFooterStyle ] [ footer ]

                Nothing ->
                    div [] []
    in
        div [ style panelContainerStyle ]
            [ panelHeader
            , body
            , panelFooter
            ]


panelHeaderStyle =
    [ ( "display", "flex" )
    , ( "justify-content", "space-between" )
    , ( "align-items", "center" )
    , ( "margin", "8px 0px 8px 0px" )
    , ( "padding-bottom", "8px" )
    , ( "border-bottom", "1px solid #EBF0F5" )
    ]


panelHeaderItemStyle =
    [ ( "font-family", "Roboto-Medium" )
    , ( "font-size", "12px" )
    , ( "color", "#9B9EA7" )
    , ( "letter-spacing", "0" )
    , ( "margin", "4px 12px 0px 12px" )
    ]

panelContainerStyle =
    [ ( "margin", "4px 8px 4px 8px" )
    , ( "background", "#FFFFFF" )
    , ( "box-shadow", "0 2px 4px 0 #D2D6DF" )
    ]

panelBodyStyle =
    [ ( "margin", "4px 8px 4px 8px" )
    , ( "background", "#FFFFFF" )
    , ( "box-shadow", "0 2px 4px 0 #D2D6DF" )
    ]


panelFooterStyle =
    [ ( "padding-bottom", "8px" )
    , ( "border-top", "1px solid #EBF0F5" )
    ]


navbarView : NavbarModel -> Html msg
navbarView navbar =
    div [ style styles.navContainer ]
        [ div [ style [ ( "margin-left", "16px" ) ] ] [ logo ]
        , div [] [ viewRightItems navbar.rightItems ]
        ]


viewRightItems : Maybe RightItems -> Html msg
viewRightItems rightItems =
    div [ style [ ( "display", "flex" ), ( "align-items", "center" ), ( "margin", "8px" ) ] ]
        [ rightItemBox (iconBell [ { message = "Do stuff" } ])
        , rightItemBox
            (case rightItems of
                Just rightItems ->
                    case rightItems.avatar of
                        Just avatar ->
                            div [] [ viewAvatar avatar ]

                        Nothing ->
                            div [] [ text "not showing an avatar?!" ]

                Nothing ->
                    div [] [ text "No right items!" ]
            )
        , rightItemBox viewHamburgerMenu
        ]


rightItemBox : Html msg -> Html msg
rightItemBox htmlMsg =
    div [ style [ ( "padding", "0px 8px 0px 8px" ) ] ] [ htmlMsg ]


viewAvatar : Avatar -> Html msg
viewAvatar url =
    case url of
        Nothing ->
            text "Error Loading Avatar"

        Just loc ->
            img [ style [ ( "width", "40px" ), ( "border-radius", "50%" ) ], src loc ] []


viewHamburgerMenu : Html msg
viewHamburgerMenu =
    div [ style [ ( "display", "flex" ), ( "flex-direction", "column" ) ] ]
        [ hamburgerBarItem
        , hamburgerBarItem
        , hamburgerBarItem
        ]


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



-- CSS STYLES


type alias Styles =
    List ( String, String )


styles : { container : Styles, navContainer : Styles }
styles =
    { container =
        [ ( "display", "flex" )
        , ( "justify-content", "space-between" )
        ]
    , navContainer =
        [ ( "display", "flex" )
        , ( "justify-content", "space-between" )
        , ( "align-items", "center" )
        , ( "width", "100vw" )
        , ( "z-index", "1030" )
        , ( "height", "56px" )
        , ( "margin-bottom", "8px" )
        , ( "background", "#FFFFFF" )
        , ( "box-shadow", "0 4px 8px 0 #D2D6DF" )
        ]
    }
