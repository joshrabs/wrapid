module Client.PAPortal.Pages.Schedule.Task exposing (..)

import Html
import Html exposing (Html)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Client.PAPortal.Pages.Schedule.Task.Types exposing (..)

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
                      , startTime : Time msg
                      , endTime : Time msg
                      , setting : Setting msg
                      , timeOfDay : TimeOfDay msg
                      , extras : List (Extra msg)
                      }

type alias Task msg = Renderable (Data msg) (Html msg) {}

create : Data msg -> Task msg
create = Renderable.create render

render : Data msg -> Html msg
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


        
gen : String -> Task msg
gen n = create { type_ = Type.create Type.Wardrobe
               , title = Title.create ("Title " ++ n)
               , desc = Description.create ("Description " ++ n)
               , startTime = Time.create "8:00"
               , endTime = Time.create "9:00"
               , setting = Setting.create Setting.Interior
               , timeOfDay = TimeOfDay.create TimeOfDay.Day
               , extras = []
                     
               }

asInput : Data msg -> Html msg
asInput { type_, title, desc, startTime, endTime, setting, timeOfDay, extras } =
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

testTask = gen "Test"
        
main : Html msg
main = asInput testTask.data
