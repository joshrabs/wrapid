module Client.PAPortal.Pages.Schedule.Task.Description exposing (..)

import Html
import Html exposing (Html)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Html.Attributes as Attr

type alias Data = String
type alias Description msg = Renderable Data (Html msg) {}

create : Data -> Description msg
create = Renderable.create render

render : Data -> Html msg
render data = Html.text data
    

input : Data -> Html msg
input data =
    let attributes = [ Attr.type_ "text"
                     , Attr.name "title"
                     ]
    in Html.textarea attributes [Html.text data]
