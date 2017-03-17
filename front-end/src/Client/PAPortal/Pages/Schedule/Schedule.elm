module Client.PAPortal.Pages.Schedule.Schedule exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)

import Common.Renderable as Renderable
import Common.Renderable exposing (Renderable)

import Client.PAPortal.Pages.Schedule.Task as Task
import Client.PAPortal.Pages.Schedule.Task exposing (Task)

type alias Data = List Task
                            
type alias Schedule = Renderable Data (Html Message) {}

create : Data -> Schedule
create = Renderable.create render

render : Data -> Html Message
render tasks =
    let attributes = []
        body = List.map Renderable.doRender tasks
    in Html.table attributes body

testSchedule : Schedule
testSchedule = create (List.map Task.gen ["1", "2", "3", "4"])

main = Renderable.doRender testSchedule
