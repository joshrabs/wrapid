module Client.PAPortal.Pages.Schedule exposing (..)

import Client.PAPortal.Pages.Schedule.Model as Model
import Client.PAPortal.Pages.Schedule.Update as Update
import Client.PAPortal.Pages.Schedule.View as View
import Client.PAPortal.Pages.Schedule.Message as Message


type alias Model = Model.Model
type alias Msg = Message.Message

initModel = Model.initModel
update = Update.update
view = View.view            


