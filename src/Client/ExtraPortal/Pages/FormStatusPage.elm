module Client.ExtraPortal.Pages.FormStatusPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, src)

import Assets.Icons.CheckedCircleIcon exposing (viewCheckedCircle)
import Assets.Icons.ForwardArrow exposing (viewForwardArrow)

type alias FormStatuses = List FormStatus

type alias FormStatus =
  { formName: String
   ,completedDt: String
   ,completedTs: String
   ,imgSrc: String
  }

viewFormStatusPage: FormStatuses -> Html msg
viewFormStatusPage statuses =
  div [style [("margin", "8px")]]
    [
      div [style [
        ("display", "flex"), ("flex-direction", "column")
      ]]
      [
        span [style [
          ("font-family", "Roboto-Bold")
          ,("font-size", "32px")
          ,("color", "#282C35")
          ,("letter-spacing", "0")
        ]]
        [text "Your Completed Forms"]
      ]
      ,span [style [
        ("display", "flex")
        ,("align-items", "center")
        ,("font-family", "Roboto")
        ,("font-size", "16px")
        ,("letter-spacing", "0")
        ,("line-height", "20px")
        ,("margin", "8px 0px 4px 0px")
       ]]
       [text "Go to daily tasks", viewForwardArrow "24" "24"]
      ,div [] (List.map (\item -> viewFormStatusIcon item) statuses)
    ]



viewFormStatusIcon: FormStatus -> Html msg
viewFormStatusIcon item =
  div [style [
     ("display", "inline-flex")
    ,("flex-direction", "column")
    ,("background", "#FFFFFF")
    ,("box-shadow", "0 8px 8px 0 rgba(210,214,223,0.70)")
    ,("margin", "8px 16px 8px 0px")
  ]]
  [
    img [src "../../../Assets/Images/theraccounATgmailDOTcom-EFS.jpg"
        , style [("width", "165px"), ("height", "118px")]] []
    ,div [style
      [("display", "flex")
      , ("flex-direction", "column")
      , ("margin", "8px")
      ]]
      [
        span [style formNameCss] [text item.formName]
        ,div [style [("display", "flex"), ("align-items", "center"), ("margin", "2px 0px 2px 0px")]]
        [
           div [style [("width", "24px"), ("height", "24px")]] [viewCheckedCircle "24" "24"]
          , span [style completedDtCss] [text ("Completed. " ++ item.completedDt)]
        ]

        , span [style completedDtCss] [text item.completedTs]
      ]

  ]


formNameCss =
  [
    ("font-family", "Roboto-Regular")
    ,("font-size", "14px")
    ,("margin-bottom", "2px")
  ]

completedDtCss =
  [
     ("font-family", "Roboto-Regular")
    ,("font-size", "12px")
    ,("color", "#9B9EA7")
    ,("letter-spacing", "0")
    ,("margin-left", "2px")
  ]
