module Client.PAPortal.Pages.Schedule.Task.Setting exposing (..)

import Html
import Html exposing (Html)

import Html.Attributes as Attr

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)


type Data = Interior | Exterior
type alias Setting msg = Renderable Data (Html msg) {}

create : Data -> Setting msg
create = Renderable.create render

render : Data -> Html msg
render = Html.text << toString 

radio : String -> Bool -> Html msg
radio value selected =
    let attributes = [ Attr.type_ "radio"
                     , Attr.name "setting"
                     , Attr.value value
                     , Attr.checked selected
                     ]
        input = Html.input attributes []
    in Html.div [] [input, Html.text value]
         
input : Maybe Data -> Html msg
input selected =
    let attributes = []
        helper n = radio (Basics.toString n) (selected == Just n)
        body = List.map helper [ Interior, Exterior ]
    in Html.div attributes body
    
    
main = input (Just Exterior)
