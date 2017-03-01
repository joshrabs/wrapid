port module Ports exposing (..)

import Client.PAPortal.Types exposing (Profile)
import Client.ExtraPortal.Types exposing (ExtraInfo, Day, UserId, Schedule)

port getAllExtraInfo : (String) -> Cmd msg

port receiveAllExtraInfo : (List Profile -> msg) -> Sub msg



port getExtraInfo : ( UserId, Day ) -> Cmd msg
port receiveExtraInfo : (ExtraInfo -> msg) -> Sub msg
