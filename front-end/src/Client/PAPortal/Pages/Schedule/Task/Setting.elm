module Client.PAPortal.Pages.Schedule.Task.Setting exposing (..)

import Html
import Html exposing (Html)

import Html.Attributes as Attr
import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)


type Data = Interior | Exterior
type alias Setting = Renderable Data (Html Message) {}

create : Data -> Setting
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
        body = List.map helper [ Interior, Exterior ]
    in Html.div attributes body
    
    
main = input Exterior
