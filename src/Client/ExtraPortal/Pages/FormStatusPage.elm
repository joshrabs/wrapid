module Client.ExtraPortal.Pages.FormStatusPage exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

type alias FormStatuses = String

viewFormStatusPage: FormStatuses -> Html msg
viewFormStatusPage statuses =
  div [style [("margin", "8px")]] [text "Welcome to the form status page!"]
