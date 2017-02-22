port module Ports exposing (..)

import Client.PAPortal.Types exposing (Profile)

port getAllExtraInfo : (String) -> Cmd msg

port receiveAllExtraInfo : (List Profile -> msg) -> Sub msg
