module Client.PAPortal.Pages.Schedule.Task exposing (Data, Task, create, new)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Widget exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget

import Client.PAPortal.Pages.Schedule.Task.Types exposing (..)

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Task.Description as Description
import Client.PAPortal.Pages.Schedule.Task.Extra as Extra
import Client.PAPortal.Pages.Schedule.Task.Setting as Setting
import Client.PAPortal.Pages.Schedule.Task.Time as Time
import Client.PAPortal.Pages.Schedule.Task.TimeOfDay as TimeOfDay
import Client.PAPortal.Pages.Schedule.Task.Title as Title
import Client.PAPortal.Pages.Schedule.Task.Type as Type

type alias Data msg = { type_ : Type msg
                      , title : Title msg 
                      , desc : Description msg
                      , time : Time msg
                      , setting : Setting msg
                      , timeOfDay : TimeOfDay msg
                      , extras : List (Extra msg)
                      }

type alias Task msg = Widget (Data msg) msg

create : Data msg -> Task msg
create = Widget.create functions

functions : Widget.Functions (Data msg) msg
functions = { default |
              render = render_
            , input = input_
            , validate = validate_
            }

render_ : Data msg -> Html msg
render_ { title, type_, desc, time, setting, timeOfDay, extras } =
    let attributes = []
        body  = 
            [ render type_
            , render title
            , render desc
            , render time
            , render setting
            , render timeOfDay
            , List.map render extras |> Html.div []
            ]
    in Html.div attributes body


gen : String -> Task msg
gen n = create { type_ = Type.create Type.Wardrobe
               , title = Title.create ("Title " ++ n)
               , desc = Description.create ("Description " ++ n)
               , time = Time.create {startTime = "8:00", endTime = "9:00"}
               , setting = Setting.create Setting.Interior
               , timeOfDay = TimeOfDay.create TimeOfDay.Day
               , extras = [ Extra.gen "Bob", Extra.gen "Sally", Extra.gen "Frank" ]
                     
               }

input_ : Data msg -> Html Message
input_ { type_, title, desc, time, setting, timeOfDay, extras } =
    let attributes = []
        body = [ input type_
               , input title
               , input desc
               , input time
               , input setting
               , input timeOfDay
               , Html.div [] (List.map (\d -> input d) extras)
               ]
    in Html.div attributes <| List.intersperse (Html.br [] []) body

new : Task msg
new = create { type_ = Type.create Type.Wardrobe
             , title = Title.create ("")
             , desc = Description.create ("")
             , time = Time.create {startTime = "", endTime = ""}
             , setting = Setting.create Setting.Interior
             , timeOfDay = TimeOfDay.create TimeOfDay.Day
             -- TODO: Load Available Extras?
             , extras = [ Extra.gen "Bob", Extra.gen "Sally", Extra.gen "Frank" ]
             }

                          
validate_ : Data msg -> Maybe (List String)
validate_ { title, desc, time } =
    let errors = [ validate title
                 , validate desc
                 , validate time
                 ]
               |> List.foldr (\el acc -> if el == Nothing then acc else (Maybe.withDefault [] el) ++ acc) []               
    in case errors of
           [] -> Nothing
           errors -> Just errors
