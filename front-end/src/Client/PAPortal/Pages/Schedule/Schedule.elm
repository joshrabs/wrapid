module Client.PAPortal.Pages.Schedule.Schedule exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Renderable as Renderable
import Client.PAPortal.Pages.Schedule.Renderable exposing (Renderable)

import Client.PAPortal.Pages.Schedule.Task as Task
import Client.PAPortal.Pages.Schedule.Task exposing (Task)

type alias Data msg = List (Task msg) 
                            
type alias Schedule msg = Renderable (Data msg) (Html msg) {}

create : Data msg -> Schedule msg
create = Renderable.create render

render : Data msg -> Html msg
render tasks =
    let attributes = []
        body = List.map Renderable.doRender tasks
    in Html.table attributes body

testSchedule : Schedule msg
testSchedule = create (List.map Task.gen ["1", "2", "3", "4"])

main = Renderable.doRender testSchedule
