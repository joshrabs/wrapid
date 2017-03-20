module Client.PAPortal.Pages.Schedule.Task.Description exposing (Data, Description, create)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Widget exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget

import Html.Attributes as Attr

type alias Data = String
type alias Description msg = Widget Data msg

create : Data -> Description msg
create = Widget.create functions

functions : Widget.Functions Data msg
functions = { render = render_
            , input = input_
            , validate = validate_
            }
               
render_ : Data -> Html msg
render_ data = Html.text data
    

-- TODO: This code compiles but dies in the browser:
--
{-              
input : Data -> Html msg
input data =
    let attributes = [ Attr.type_ "text"
                     , Attr.name "title"
                     ]
    in Html.textarea attributes [Html.text data]
-}

input_ : Data -> Html Message
input_ data =
    let attributes = [ Attr.name "title"
                     ]
    in Html.textarea attributes [Html.text data]
        
validate_ : Data -> Maybe (List String)
validate_ data =
    case data of
        "" -> Just ["The description may not be blank"]
        _ -> Nothing
