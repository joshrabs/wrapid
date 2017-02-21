module Common.Styles exposing (..)


type alias Styles =
    List ( String, String )


baseTabStyle : Styles
baseTabStyle =
    [ ( "padding", "8px" )
    , ( "font-size", "14px" )
    , ( "font-family", "Roboto-Regular" )
    , ( "font-family", "Roboto-Regular" )
    , ( "color", "#6D717A" )
    ]


selectedTabStyle : Styles
selectedTabStyle =
    List.concat
        [ baseTabStyle
        , [ ( "border-bottom", "2px solid black" )
          , ( "color", "black" )
          , ( "font-family", "Roboto-Medium" )
          ]
        ]


headerContainerStyle : Styles
headerContainerStyle =
    [ ( "margin", "16px" )
    , ( "display", "inline-flex" )
    , ( "flex-direction", "column" )
    ]


headerDateStyle : Styles
headerDateStyle =
    [ ( "font-family", "Roboto-Regular" )
    , ( "font-size", "12px" )
    , ( "color", "#6D717A" )
    ]


headerCompanyStyle : Styles
headerCompanyStyle =
    [ ( "font-family", "Roboto-Regular" )
    , ( "font-size", "16px" )
    , ( "margin", "8px 0px 8px 0px" )
    , ( "color", "#282C35" )
    , ( "letter-spacing", "0" )
    ]


navContainer : Styles
navContainer =
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


navbarRight : Styles
navbarRight =
    [ ( "display", "flex" )
    , ( "align-items", "center" )
    , ( "margin", "8px" )
    ]
