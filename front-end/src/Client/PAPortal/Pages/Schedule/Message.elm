module Client.PAPortal.Pages.Schedule.Message exposing (..)

type Message = StartAddTask
             | SubmitNewTask
             | CloseModal
               
type Modal = AddTask
           | Error (List String)
    
