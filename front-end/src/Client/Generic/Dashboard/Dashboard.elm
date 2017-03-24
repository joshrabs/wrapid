module Client.Generic.Dashboard.Dashboard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Client.Generic.Dashboard.IconBell exposing (iconBell)
import Client.Generic.Dashboard.WrapidLogo exposing (logo)
import Client.Generic.Dashboard.LeftSideMenu exposing (viewLeftSideMenu, SideMenuTabInput)

--MODEL


type alias Model msg =
    { navbar : NavbarModel
    , leftMenuTabs : Maybe (List (SideMenuTabInput msg))
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

defaultAvatar =
    "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"

--UPDATE


type Msg
    = NoOp



-- VIEW


view : Model msg -> Html msg
view model =
    div []
        [ navbarView model.navbar
        , case model.leftMenuTabs of
            Just leftTabs -> viewLeftSideMenu leftTabs
            Nothing -> div [] []
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
    [ ( "background", "#FFFFFF" )
    , ( "box-shadow", "0 2px 4px 0 #D2D6DF" )
    ]

panelBodyStyle =
    [ ( "background", "#FFFFFF" )
    , ( "box-shadow", "0 2px 4px 0 #D2D6DF" )
    ]


panelFooterStyle =
    [ ( "padding-bottom", "8px" )
    , ( "border-top", "1px solid #EBF0F5" )
    ]


navbarView : NavbarModel -> Html msg
navbarView navbar =
    div [ style styles.navContainer ]
        [ div [ style [ ( "margin-left", "16px" ), ("display", "flex"), ("align-items", "center") ] ]
          [ logo
          , div [ style [
            ("margin-left", "123px")
            , ("font-family", "Roboto-Regular")
            , ("font-size", "24px")
            , ("color", "white")
            ]]
            [text "Runabetterset - Day 1"]
          ]
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
                            div [] [ ]

                Nothing ->
                    div [] []
            )
        , rightItemBox viewHamburgerMenu
        ]


rightItemBox : Html msg -> Html msg
rightItemBox htmlMsg =
    div [ style [ ( "padding", "0px 8px 0px 8px" ) ] ] [ htmlMsg ]


viewAvatar : Avatar -> Html msg
viewAvatar url =
    img [ style [ ( "width", "40px" ), ("height", "40px"), ( "border-radius", "50%" ) ]
      , src
          (case url of
            Nothing -> defaultAvatar
            Just loc -> loc
          )
        ]
        []



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
            , ( "background-color", "white" )
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
        , ( "background", "#23232F" )
        , ( "box-shadow", "0 4px 8px 0 #D2D6DF" )
        ]
    }
