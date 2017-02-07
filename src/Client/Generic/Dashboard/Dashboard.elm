module Client.Generic.Dashboard.Dashboard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import WrapidLogo exposing (logo)

--MODEL

type alias Model = {
  navbar: NavbarModel
}

type alias NavbarModel = {
  rightItems: Maybe RightItems
}

type alias RightItems = {
  avatar: Maybe Avatar
}

type alias Avatar = Maybe String

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
    div [] [logo]
    ,div [] [viewRightItems navbar.rightItems]

  ]

viewRightItems: Maybe RightItems -> Html msg
viewRightItems rightItems =
  case rightItems of
    Just rightItems ->
      case rightItems.avatar of
        Just avatar -> div [] [viewAvatar avatar]
        Nothing -> div [] [text "not showing an avatar?!"]
    Nothing -> div [] [text "No right items!"]


viewAvatar : Avatar -> Html msg
viewAvatar url =
    case url of
        Nothing ->
            text "Error Loading Avatar"

        Just loc ->
            img [ style [("width", "40px")], src loc ] []


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
      , ( "width", "100vw" )
      , ( "border", "4px solid #337AB7")
      ]
  }
