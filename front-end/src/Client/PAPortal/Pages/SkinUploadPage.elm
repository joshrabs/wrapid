module Client.PAPortal.Pages.SkinUploadPage exposing (..)

import Html exposing (Html, div, text, input)
import Html.Attributes exposing (style, type_, accept, id)
import Html.Events exposing (onClick)

--Update
type Msg = UploadSkin | CreateSkin

--View
view: Html Msg
view =
  div [style [("display", "flex"), ("justify-content", "center"), ("align-items", "center")]]
  [
    div [onClick UploadSkin, style (baseUploadBoxStyle ++ [("background", "#ab47bc")])]
    [text "Upload Skin"]
  , input
      [ style [ ( "display", "none" ) ], id "NodeSkinCSVUpload", type_ "file", accept "*" ]
      []
  , div [] [text "Or"]
  , div [onClick CreateSkin, style (baseUploadBoxStyle ++ [("background", "rgb(80, 227, 194)")])]
    [text "Create New Skin"]
  ]


baseUploadBoxStyle: List (String, String)
baseUploadBoxStyle =
  [
    ("width", "300px")
    , ("height", "300px")
    , ("display", "flex")
    , ("align-items", "center")
    , ("justify-content", "center")
    , ("margin", "10px")
    , ("font-family", "Roboto-Medium")
    , ("font-size", "16px")
    , ("color", "white")
    ,("box-shadow", "0 2px 4px 0 rgba(155,158,167,0.50)")
  ]
