module Client.Generic.Dashboard.LeftSideMenu exposing (..)

import Html exposing (Html, div, i, text, span)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)

type alias SideMenuTabInput msg =
  {isSelected: Bool
  , iconName:String
  , text:String
  , onClickMsg: msg
  }

viewLeftSideMenu: List (SideMenuTabInput msg) -> Html msg
viewLeftSideMenu tabs =
  div [style [
     ("min-width", "120px")
    ,("height", "100vh")
    ,("background", "#23232F")
  ]]
  (List.map
    (\t ->
      let
        sideTabStyle = sideMenuBlockStyle t.isSelected
      in
        div [onClick t.onClickMsg, style sideTabStyle.container]
          [Html.i [class t.iconName, style sideTabStyle.icon] []
          ,span [style sideTabStyle.text] [text t.text]
          ]

    )
    tabs
  )

sideMenuBlockStyle isSelected =
  {container =
    [("display", "flex")
    ,("flex-direction", "column")
    ,("align-items", "center")
    ,("justify-content", "center")
    ,("height", "100px")
    ,("background", if isSelected then "white" else "inherit")
    ]

  ,icon =
    [("color", if isSelected then "black" else "white")
    ,("font-size", "24px")
    ]

  ,text =
    [("color", if isSelected then "black" else "white")
    ,("font-size", "14px")
    ,("font-family", "Roboto-Regular")
    ,("margin-top", "8px")
    ]
  }
