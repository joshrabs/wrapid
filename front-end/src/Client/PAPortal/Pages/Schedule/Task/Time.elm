module Client.PAPortal.Pages.Schedule.Task.Time exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Html.Attributes as Attr

type alias Data = String
type alias Time = Renderable Data (Html Message) {}

create : Data -> Time
create = Renderable.create render

render : Data -> Html Message
render data = Html.text data
    

input : String -> Data -> Html Message
input label data =
    let attributes = [ Attr.type_ "text"
                     , Attr.name "title"
                     , Attr.value data
                     ]
    in Html.div [] [ Html.label [] [Html.text label]
                   , Html.input attributes []]

validate : Time -> Time -> Maybe (List String)
validate startTime endTime =
    let errors = [ validate_ "Start Time" startTime
                 , validate_ "End Time" endTime
                 ] 
                 |> List.filter ((/=) "") 
    in if List.isEmpty errors then Nothing else Just errors
                 
validate_ : String -> Time -> String
validate_ label time =
    case time.data of
        "" -> label ++ " is required."
        _ -> ""
