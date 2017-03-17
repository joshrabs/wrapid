module Client.PAPortal.Pages.Schedule.Model exposing (..)
import Client.PAPortal.Pages.Schedule.Message exposing (..)
import Client.PAPortal.Pages.Schedule.Task as Task

--MODEL
type alias Model =  { task : Maybe Task.Task
                    , modals : List Modal
                    }

    
initModel: Model
initModel = { task = Nothing
            , modals = []
            }
