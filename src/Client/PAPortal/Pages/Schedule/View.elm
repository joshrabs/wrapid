module Client.PAPortal.Pages.Schedule.View exposing (..)

import Client.PAPortal.Pages.Schedule.Model exposing (..)
import Client.PAPortal.Pages.Schedule.Update exposing (..)

import Html exposing (Html, div, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict exposing (Dict)
import Dict

import Common.Styles.TextStyles exposing (regularBolder, regularLight, headerTitleStyle)
import Common.Styles.ButtonStyles exposing (blackButtonStyle)

import Material.Icon as Icon exposing (view)


--VIEW
view: Model -> Html Msg
view model = div [] []
