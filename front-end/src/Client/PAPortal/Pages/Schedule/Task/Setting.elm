module Client.PAPortal.Pages.Schedule.Task.Setting exposing (..)

import Html
import Html exposing (Html)

import Html.Attributes as Attr
import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Widget exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget


type Data = Interior | Exterior
type alias Setting msg = Widget Data msg

create : Data -> Setting msg
create = Widget.create functions

functions = { default |
              render = render_
            , input = input_
            }

render_ : Data -> Html msg
render_ = Html.text << toString 

radio : String -> Bool -> Html Message
radio value selected =
    let attributes = [ Attr.type_ "radio"
                     , Attr.name "setting"
                     , Attr.value value
                     , Attr.checked selected
                     ]
        input = Html.input attributes []
    in Html.div [] [input, Html.text value]
         
input_ : Data -> Html Message
input_ selected =
    let attributes = []
        helper n = radio (Basics.toString n) (selected == n)
        body = List.map helper [ Interior, Exterior ]
    in Html.div attributes body
    
    
