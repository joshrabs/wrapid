module Client.PAPortal.Pages.Schedule.Task.Title exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Html.Attributes as Attr

type alias Data = String
type alias Title = Renderable Data (Html Message) {}

create : Data -> Title
create = Renderable.create render

render : Data -> Html Message
render data = Html.text data
    
input : Data -> Html Message
input data =
    let attributes = [ Attr.type_ "text"
                     , Attr.name "title"
                     , Attr.value data
                     ]
    in Html.input attributes []

validate : Title -> Maybe (List String)
validate toValidate =
    case toValidate.data of
        "" -> Just ["You must specify a title"]
        _ -> Nothing
