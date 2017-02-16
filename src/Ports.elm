port module Ports exposing (..)

import Client.PAPortal.Types exposing (Profile)


port getAllProfiles : () -> Cmd msg


port receiveNames : (List Profile -> msg) -> Sub msg
