module Client.PAPortal.Pages.Schedule.Task.Types exposing (..)

import Client.PAPortal.Pages.Schedule.Task.Description as Description
import Client.PAPortal.Pages.Schedule.Task.Extra as Extra
import Client.PAPortal.Pages.Schedule.Task.Setting as Setting
import Client.PAPortal.Pages.Schedule.Task.Time as Time
import Client.PAPortal.Pages.Schedule.Task.TimeOfDay as TimeOfDay
import Client.PAPortal.Pages.Schedule.Task.Title as Title
import Client.PAPortal.Pages.Schedule.Task.Type as Type

type alias Description msg = Description.Description msg
type alias Extra msg = Extra.Extra msg
type alias Setting msg = Setting.Setting msg
type alias Time msg = Time.Time msg
type alias TimeOfDay msg = TimeOfDay.TimeOfDay msg
type alias Title msg = Title.Title msg
type alias Type msg = Type.Type msg
