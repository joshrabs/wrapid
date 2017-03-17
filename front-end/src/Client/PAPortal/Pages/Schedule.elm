module Client.PAPortal.Pages.Schedule exposing (..)

import Client.PAPortal.Pages.Schedule.Model as Model
import Client.PAPortal.Pages.Schedule.Update as Update
import Client.PAPortal.Pages.Schedule.View as View

type alias Model = Model.Model
type alias Msg = Update.Msg
    
initModel = Model.initModel
update = Update.update
view = View.view            


