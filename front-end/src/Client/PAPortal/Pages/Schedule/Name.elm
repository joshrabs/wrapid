module Client.PAPortal.Pages.Schedule.Name exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Renderable as Renderable
import Client.PAPortal.Pages.Schedule.Renderable exposing (Renderable)


type alias Data = String
type alias Name msg = Renderable Data (Html msg) {}

create : Data -> Name msg
create = Renderable.create render

render : Data -> Html msg
render data = Html.text data
    


