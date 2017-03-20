module Client.PAPortal.Pages.Schedule.Update exposing (..)

import Client.PAPortal.Pages.Schedule.Model exposing (..)
import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Task as Task

import Client.PAPortal.Pages.Schedule.Widget as Widget

import Dict exposing (Dict)
import Dict

--UPDATE

update: Message -> Model msg -> Model msg
update msg model =
    case msg of
        StartAddTask -> { model |
                          modals = AddTask::model.modals
                        }

        SubmitNewTask ->
            case model.task of
                Nothing -> { model | modals = Error ["You must specify the task."] :: model.modals }
                Just task ->
                    let result = Widget.validate task
                    in case result of
                           Nothing -> addNewTask model
                           Just errors -> { model | modals = Error errors :: model.modals }

        CloseModal -> case model.modals of
                          [] -> model
                          (_::xs) -> { model | modals = xs }
                                  

addNewTask model = model
