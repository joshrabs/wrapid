port module Ports exposing (..)

import Client.PAPortal.Types as PATypes exposing (Skin, ExtraInfo)
import Client.ExtraPortal.Types as EPTypes exposing (ExtraInfo, Day, UserId, Schedule, ScheduleItem)

port getAllExtraInfo : (String) -> Cmd msg
port receiveAllExtraInfo : (List PATypes.ExtraInfo -> msg) -> Sub msg

port fetchDailySkin : (String) -> Cmd msg
port receiveDailySkin : (Maybe Skin -> msg) -> Sub msg

port uploadSkin : Skin -> Cmd msg
receiveUploadedSkin = receiveDailySkin

port addScheduleItem : (UserId, ScheduleItem) -> Cmd msg


port getExtraInfo : ( UserId, Day ) -> Cmd msg
port receiveExtraInfo : (EPTypes.ExtraInfo -> msg) -> Sub msg


port uploadAvatar : (UserId) -> Cmd msg
