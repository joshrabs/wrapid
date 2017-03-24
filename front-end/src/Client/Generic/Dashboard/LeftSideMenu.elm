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
    ("position", "fixed")
    ,("left", "0")
    ,("top", "56")
    ,("width", "120px")
    ,("height", "100vh")
    ,("background", "#23232F")
  ]]
  (List.map
    (\t ->
      div [onClick t.onClickMsg, style sideMenuBlockStyle.container]
        [Html.i [class t.iconName, style sideMenuBlockStyle.icon] []
        ,span [style sideMenuBlockStyle.text] [text t.text]
        ]
    )
    tabs
  )

sideMenuBlockStyle =
  {container =
    [("display", "flex")
    ,("flex-direction", "column")
    ,("align-items", "center")
    ,("justify-content", "center")
    ,("height", "100px")
    ]

  ,icon =
    [("color", "white")
    ,("font-size", "24px")
    ]

  ,text =
    [("color", "white")
    ,("font-size", "14px")
    ,("font-family", "Roboto-Regular")
    ,("margin-top", "8px")
    ]
  }
