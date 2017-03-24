module Client.Generic.Dashboard.LeftSideMenu exposing (..)

import Html exposing (Html, div, i, text, span)
import Html.Attributes exposing (style, class)

viewLeftSideMenu: Html msg
viewLeftSideMenu =
  div [style [
    ("position", "fixed")
    ,("left", "0")
    ,("top", "56")
    ,("width", "120px")
    ,("height", "100vh")
    ,("background", "#23232F")
  ]]
  [div [style sideMenuBlockStyle.container]
    [Html.i [class "fa fa-video-camera", style sideMenuBlockStyle.icon] []
    ,span [style sideMenuBlockStyle.text] [text "Live Monitor"]
    ]
  ,div [style sideMenuBlockStyle.container]
    [Html.i [class "fa fa-calendar", style sideMenuBlockStyle.icon] []
    ,span [style sideMenuBlockStyle.text] [text "Daily Calendar"]
    ]
  ,div [style sideMenuBlockStyle.container]
    [Html.i [class "fa fa-users", style sideMenuBlockStyle.icon] []
    ,span [style sideMenuBlockStyle.text] [text "Skins"]
    ]
  ,div [style sideMenuBlockStyle.container]
    [Html.i [class "fa fa-video-camera", style sideMenuBlockStyle.icon] []
    ,span [style sideMenuBlockStyle.text] [text "Wrap"]
    ]
  ]

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
