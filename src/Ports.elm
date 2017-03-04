port module Ports exposing (..)

import Client.PAPortal.Types exposing (Profile)
import Client.ExtraPortal.Types exposing (ExtraInfo, Day, UserId, Schedule, ScheduleItem)

port getAllExtraInfo : (String) -> Cmd msg

port receiveAllExtraInfo : (List ExtraInfo -> msg) -> Sub msg

port addScheduleItem : (UserId, ScheduleItem) -> Cmd msg


port getExtraInfo : ( UserId, Day ) -> Cmd msg
port receiveExtraInfo : (ExtraInfo -> msg) -> Sub msg
