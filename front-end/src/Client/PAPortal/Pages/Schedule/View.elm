module Client.PAPortal.Pages.Schedule.View exposing (..)

import Client.PAPortal.Pages.Schedule.Task as Task
import Client.PAPortal.Pages.Schedule.Task exposing (Task)
import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Model exposing (..)
import Client.PAPortal.Pages.Schedule.Update exposing (..)
import Client.PAPortal.Pages.Schedule.Widget as Widget


import Html exposing (Html)
import Html

import Html.Attributes as Attr
import Html.Events as Events


--VIEW
view: Model msg -> Html Message
view model =
    let attributes = []
        modals = List.map (getModal model) model.modals
        body = (addTaskButton StartAddTask)::modals
    in 
        Html.div attributes body

button label attributes onClick =
    let body = [Html.text label]
        attributes_ = (Events.onClick onClick)::attributes
    in Html.button attributes_ body
            
addTaskButton = button "Add Task" []
closeButton label = button label [] CloseModal

getModal model modal =
    case modal of
        AddTask -> createTaskModal model.task
        Error errors -> createErrorModal errors

createErrorModal : List String -> Html Message
createErrorModal errors =
    let attributes = []
        body = Html.h2 [] [Html.text "Error"] ::
               (List.map (\e -> Html.p [] [Html.text e]) errors)
               
    in Html.div [] ( body ++ [closeButton "Okay"] )        
                
createTaskModal : Maybe (Task msg) -> Html Message
createTaskModal task =
    let task_ = case task of
                    Nothing -> Task.new
                    Just task -> task
        attributes = []
        body = [ Html.h2 [] [Html.text "Add Task"]
               , Widget.input task_
               , addTaskButton SubmitNewTask
               , closeButton "Cancel"
               ]
    in Html.div attributes body
