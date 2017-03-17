module Client.PAPortal.Pages.Schedule.Task exposing (..)

import Html
import Html exposing (Html)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Client.PAPortal.Pages.Schedule.Task.Types exposing (..)

import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Task.Description as Description
import Client.PAPortal.Pages.Schedule.Task.Extra as Extra
import Client.PAPortal.Pages.Schedule.Task.Setting as Setting
import Client.PAPortal.Pages.Schedule.Task.Time as Time
import Client.PAPortal.Pages.Schedule.Task.TimeOfDay as TimeOfDay
import Client.PAPortal.Pages.Schedule.Task.Title as Title
import Client.PAPortal.Pages.Schedule.Task.Type as Type

type alias Data = { type_ : Type
                  , title : Title
                  , desc : Description
                  , startTime : Time
                  , endTime : Time
                  , setting : Setting
                  , timeOfDay : TimeOfDay
                  , extras : List Extra
                  }

type alias Task = Renderable Data (Html Message) {}

create : Data -> Task
create = Renderable.create render

render : Data -> Html Message
render { title, type_, desc, startTime, endTime, setting, timeOfDay, extras } =
    let attributes = []
        body  = 
            [ Renderable.doRender type_
            , Renderable.doRender title
            , Renderable.doRender desc
            , Renderable.doRender startTime
            , Renderable.doRender endTime
            , Renderable.doRender setting
            , Renderable.doRender timeOfDay
            , List.map Renderable.doRender extras |> Html.div []
            ]
    in Html.div attributes body


        
gen : String -> Task
gen n = create { type_ = Type.create Type.Wardrobe
               , title = Title.create ("Title " ++ n)
               , desc = Description.create ("Description " ++ n)
               , startTime = Time.create "8:00"
               , endTime = Time.create "9:00"
               , setting = Setting.create Setting.Interior
               , timeOfDay = TimeOfDay.create TimeOfDay.Day
               , extras = [ Extra.gen "Bob", Extra.gen "Sally", Extra.gen "Frank" ]
                     
               }

input : Data -> Html Message
input { type_, title, desc, startTime, endTime, setting, timeOfDay, extras } =
    let attributes = []
        body = [ Type.input type_.data
               , Title.input title.data
               , Description.input desc.data
               , Time.input "Start Time" startTime.data
               , Time.input "End Time" endTime.data
               , Setting.input setting.data
               , TimeOfDay.input timeOfDay.data
               , Html.div [] (List.map (\d -> Extra.input False d.data) extras)
               ]
    in Html.div attributes <| List.intersperse (Html.br [] []) body

new : Task
new = create { type_ = Type.create Type.Wardrobe
             , title = Title.create ("")
             , desc = Description.create ("")
             , startTime = Time.create ""
             , endTime = Time.create ""
             , setting = Setting.create Setting.Interior
             , timeOfDay = TimeOfDay.create TimeOfDay.Day
             -- TODO: Load Available Extras?
             , extras = [ Extra.gen "Bob", Extra.gen "Sally", Extra.gen "Frank" ]
             }

                          
validate : Maybe Task -> Maybe (List String)
validate toValidate =
    case toValidate of
        Nothing -> validate <| Just new
        Just { data } ->
            let errors = [ Title.validate data.title
                         , Description.validate data.desc
                         , Time.validate data.startTime data.endTime
                         ]
                         |> List.foldr (\el acc -> if el == Nothing then acc else (Maybe.withDefault [] el) ++ acc) []               
            in case errors of
                   [] -> Nothing
                   errors -> Just errors
