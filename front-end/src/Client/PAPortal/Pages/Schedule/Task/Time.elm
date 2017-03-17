module Client.PAPortal.Pages.Schedule.Task.Time exposing (..)

import Html
import Html exposing (Html)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Html.Attributes as Attr

type alias Data = String
type alias Time msg = Renderable Data (Html msg) {}

create : Data -> Time msg
create = Renderable.create render

render : Data -> Html msg
render data = Html.text data
    

input : String -> Data -> Html msg
input label data =
    let attributes = [ Attr.type_ "text"
                     , Attr.name "title"
                     , Attr.value data
                     ]
    in Html.div [] [ Html.label [] [Html.text label]
                   , Html.input attributes []]
