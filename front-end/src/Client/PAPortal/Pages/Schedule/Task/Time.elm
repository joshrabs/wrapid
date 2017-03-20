module Client.PAPortal.Pages.Schedule.Task.Time exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)

import Client.PAPortal.Pages.Schedule.Widget exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget

import Html.Attributes as Attr

type alias Data = { startTime : String
                  , endTime : String
                  }
type alias Time msg = Widget Data msg

create : Data -> Time msg
create = Widget.create functions

functions = { default |
              render = render_
            , input = input_
            , validate = validate_
            }

render_ : Data -> Html msg
render_ {startTime, endTime} = Html.text (startTime ++ " - " ++ endTime)
    
input_ : Data -> Html Message
input_ {startTime, endTime} =
    let attributes = []
        body = [ inputString "Start Time" startTime
               , inputString "End Time" endTime
               ]
    in Html.div attributes body
              
inputString : String -> String -> Html Message
inputString label data =
    let attributes = [ Attr.type_ "text"
                     , Attr.value data
                     ]
    in Html.div [] [ Html.label [] [Html.text label]
                   , Html.input attributes []]
        
validate_ : Data -> Maybe (List String)
validate_ {startTime, endTime} =
    let errors = [ validateWithLabel "Start Time" startTime
                 , validateWithLabel "End Time" endTime
                 ] 
                 |> List.filter ((/=) "") 
    in if List.isEmpty errors then Nothing else Just errors
                 
validateWithLabel : String -> String -> String
validateWithLabel label time =
    case time of
        "" -> label ++ " is required."
        _ -> ""
