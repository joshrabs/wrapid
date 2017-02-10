module Client.Generic.Dashboard.Dashboard exposing (..)

import Material
import Material.Scheme
import Material.Color as Color
import Material.Options as Options exposing (css, when)
import Material.Typography as Typography
import Material.Layout as Layout
import Material.Icon as Icon

import Html exposing (..)
import Html.Attributes exposing (..)
import Client.Generic.Dashboard.HamburgerMenu exposing (iconBell)

import WrapidLogo exposing (logo)

--MODEL

type alias Model = {
  navbar: NavbarModel
}

type alias NavbarModel = {
  rightItems: Maybe RightItems
}

type alias RightItems =
  { avatar: Maybe Avatar
  }

type alias Avatar = Maybe String
type alias NotificationBell = List Notification
type alias Notification = String

--UPDATE
type Msg = NoOp

-- VIEW

view: Model -> Html msg
view model =
  div []
  [
    navbarView model.navbar
  ]



navbarView: NavbarModel -> Html msg
navbarView navbar =
  div [style styles.navContainer]
  [
     div [style [("margin-left", "16px")]] [logo]
    ,div [] [viewRightItems navbar.rightItems]
  ]

viewRightItems: Maybe RightItems -> Html msg
viewRightItems rightItems =
  div [style [("display", "flex"), ("align-items", "center"), ("margin", "8px")]]
  [
      rightItemBox iconBell
    , rightItemBox (case rightItems of
        Just rightItems ->
          case rightItems.avatar of
            Just avatar -> div [] [viewAvatar avatar]
            Nothing -> div [] [text "not showing an avatar?!"]

        Nothing -> div [] [text "No right items!"])
    , rightItemBox viewHamburgerMenu
  ]

rightItemBox: Html msg -> Html msg
rightItemBox htmlMsg = div [style [("padding", "0px 8px 0px 8px")]] [htmlMsg]

viewAvatar : Avatar -> Html msg
viewAvatar url =
    case url of
        Nothing ->
            text "Error Loading Avatar"

        Just loc ->
            img [ style [("width", "40px"), ("border-radius", "50%")], src loc ] []


viewHamburgerMenu: Html msg
viewHamburgerMenu =
  div [style [("display", "flex"), ("flex-direction", "column")]]
  [
    hamburgerBarItem, hamburgerBarItem, hamburgerBarItem
  ]

hamburgerBarItem =
  span [
    style
    [
        ("border-radius", "1px")
      , ("background-color", "black")
      , ("width", "18px")
      , ("height", "3px")
      , ("margin", "1px 0px 2px 0px")
    ]] []

-- CSS STYLES
type alias Styles = List (String, String)
styles : { container : Styles, navContainer: Styles }
styles =
  {
    container =
      [
        ( "display", "flex")
      , ( "justify-content", "space-between")
      ],
    navContainer =
      [
        ( "display", "flex")
      , ( "justify-content", "space-between")
      , ( "align-items", "center")
      , ( "width", "100vw" )
      , ("z-index", "1030")
      , ("height", "56px")
      , ("margin-bottom", "8px")
      , ("background", "#FFFFFF")
      , ("box-shadow", "0 4px 8px 0 #D2D6DF")
      ]
  }
