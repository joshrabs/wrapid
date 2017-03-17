module Client.PAPortal.Pages.Schedule.Role exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Renderable as Renderable
import Client.PAPortal.Pages.Schedule.Renderable exposing (Renderable)


type alias Data = String
type alias Role msg = Renderable Data (Html msg) {}

create : Data -> Role msg
create = Renderable.create render

render : Data -> Html ms
render data = Html.text data
    


