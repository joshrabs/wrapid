module Client.ExtraPortal.Pages.FormStatusPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, src)

type alias FormStatuses = List FormStatus

type alias FormStatus =
  { formName: String
   ,completedTime: String
   ,imgSrc: String
  }

viewFormStatusPage: FormStatuses -> Html msg
viewFormStatusPage statuses =
  div [style [("margin", "8px")]]
    (List.map (\item -> viewFormStatusIcon item) statuses)


viewFormStatusIcon: FormStatus -> Html msg
viewFormStatusIcon item =
  div []
  [img [src item.imgSrc] []
  , text item.formName
  , text item.completedTime
  ]
