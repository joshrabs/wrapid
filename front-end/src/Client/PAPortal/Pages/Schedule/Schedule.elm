module Client.PAPortal.Pages.Schedule.Schedule exposing (..)

import Html
import Html exposing (Html)

import Client.PAPortal.Pages.Schedule.Message exposing (..)

import Client.PAPortal.Pages.Schedule.Widget exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget

import Client.PAPortal.Pages.Schedule.Task as Task
import Client.PAPortal.Pages.Schedule.Task exposing (Task)

type alias Data msg = List (Task msg)
                            
type alias Schedule msg = Widget (Data msg) msg

create : Data msg -> Schedule msg
create = Widget.create functions

functions : Widget.Functions (Data msg) msg
functions = { default |
              render = render_
            }

render_ : Data msg -> Html msg
render_ tasks =
    let attributes = []
        body = List.map render tasks
    in Html.table attributes body



