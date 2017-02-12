module Client.ExtraPortal.Pages.ProfileWizard exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)

type alias FormStatuses = String

viewProfileWizard: Maybe FormStatuses -> Html msg
viewProfileWizard statuses =
  div [style [("margin", "8px")]] [text "Welcome to the form status page!"]
