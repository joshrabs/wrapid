module Client.PAPortal.Pages.Schedule.Update exposing (..)

import Client.PAPortal.Pages.Schedule.Model exposing (..)
import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Task as Task
import Dict exposing (Dict)
import Dict

--UPDATE

update: Message -> Model -> Model
update msg model =
    case msg of
        StartAddTask -> { model |
                          modals = AddTask::model.modals
                        }

        SubmitNewTask ->
            let result = Task.validate model.task
            in case result of
                   Nothing -> addNewTask model
                   Just errors -> { model | modals = Error errors :: model.modals }

        CloseModal -> case model.modals of
                          [] -> model
                          (_::xs) -> { model | modals = xs }
                                  

addNewTask model = model
