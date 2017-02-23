module Common.Styles.TextStyles exposing (..)

type alias Style = List (String, String)

fontSizeToPx: Int -> String
fontSizeToPx s = (s |> toString) ++ "px"

regularLight: Int -> Style
regularLight fontSize =
  [ ( "padding", "8px" )
  , ( "font-size", fontSizeToPx fontSize )
  , ( "font-family", "Roboto-Regular" )
  , ( "font-family", "Roboto-Regular" )
  , ( "color", "#6D717A" )
  ]


regularBolder: Int -> Style
regularBolder fontSize =
      [ ( "font-family", "Roboto-Regular" )
      , ( "font-size", fontSizeToPx fontSize )
      , ( "margin", "8px 0px 8px 0px" )
      , ( "color", "#282C35" )
      , ( "letter-spacing", "0" )
      ]
