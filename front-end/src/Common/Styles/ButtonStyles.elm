module Common.Styles.ButtonStyles exposing (..)

type alias Style = List (String, String)

blackButtonStyle: Bool -> Style
blackButtonStyle isActive =
  let
      base =
        [("background", "#282C35")
        ,("box-shadow", "0 2px 4px 0 rgba(155,158,167,0.50)")
        ,("border-radius", "2px")
        ,("display", "flex")
        ,("justify-content", "center")
        ,("align-items", "center")
        ,("width", "117px")
        ,("height", "32px")
        ,("font-size", "12px")
        ,("font-family", "Roboto-Regular")
        ]

      active =
        [
          ("color", "white")
        ]

      inactive = [("background", "#EFF3F7"), ("color", "#D2D6DF")]
  in
    List.concat [base, (if isActive then active else inactive)]
