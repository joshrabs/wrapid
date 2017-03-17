module Client.PAPortal.Pages.Schedule.Task.Description exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Html.Attributes as Attr

type alias Data = String
type alias Description = Renderable Data (Html Message) {}

    

create : Data -> Description
create = Renderable.create render

render : Data -> Html Message
render data = Html.text data
    

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

input : Data -> Html Message
input data =
    let attributes = [ Attr.name "title"
                     ]
    in Html.textarea attributes [Html.text data]
        
validate : Description -> Maybe (List String)
validate toValidate =
    case toValidate.data of
        "" -> Just ["The description may not be blank"]
        _ -> Nothing
