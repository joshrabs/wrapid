module Client.PAPortal.Pages.Schedule.Task.Title exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Widget exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget

import Html.Attributes as Attr

type alias Data = String
type alias Title msg = Widget Data msg

create : Data -> Title msg
create = Widget.create functions

functions : Widget.Functions Data msg
functions = { default |
              render = render_
            , input = input_
            , validate = validate_
            }
         
render_ : Data -> Html msg
render_ data = Html.text data
    
input_ : Data -> Html Message
input_ data =
    let attributes = [ Attr.type_ "text"
                     , Attr.name "title"
                     , Attr.value data
                     ]
    in Html.input attributes []

validate_ : Data -> Maybe (List String)
validate_ toValidate =
    case toValidate of
        "" -> Just ["You must specify a title"]
        _ -> Nothing
