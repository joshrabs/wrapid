module Client.PAPortal.Pages.Schedule.Task.TimeOfDay exposing (..)

import Html
import Html exposing (Html)

import Html.Attributes as Attr
import Client.PAPortal.Pages.Schedule.Message exposing (..)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)


type Data = Day | Night
type alias TimeOfDay = Renderable Data (Html Message) {}

create : Data -> TimeOfDay
create = Renderable.create render

render : Data -> Html Message
render = Html.text << toString 

radio : String -> Bool -> Html Message
radio value selected =
    let attributes = [ Attr.type_ "radio"
                     , Attr.name "setting"
                     , Attr.value value
                     , Attr.checked selected
                     ]
        input = Html.input attributes []
    in Html.div [] [input, Html.text value]
         
input : Data -> Html Message
input selected =
    let attributes = []
        helper n = radio (Basics.toString n) (selected == n)
        body = List.map helper [ Day, Night ]
    in Html.div attributes body
    
    
main = input Day
