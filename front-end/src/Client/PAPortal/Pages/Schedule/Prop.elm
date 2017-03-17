module Client.PAPortal.Pages.Schedule.Prop exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Renderable as Renderable
import Client.PAPortal.Pages.Schedule.Renderable exposing (Renderable)


type alias Data = String
type alias Prop msg = Renderable Data (Html msg) {}

create : Data -> Prop msg
create = Renderable.create render

render : Data -> Html ms
render data = Html.text data
    


