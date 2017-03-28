port module Ports exposing (..)

import Client.PAPortal.Types as PATypes exposing (Skin, ExtraInfo)
import Client.ExtraPortal.Types as EPTypes exposing (ExtraInfo, Day, UserId, Schedule, ScheduleItem)
import Common.Types.Skin as Common exposing (Skin)

port getAllExtraInfo : (String) -> Cmd msg
port receiveAllExtraInfo : (List PATypes.ExtraInfo -> msg) -> Sub msg

port fetchDailySkin : (String) -> Cmd msg
port receiveDailySkin : (Maybe Common.Skin -> msg) -> Sub msg

port uploadSkin : Common.Skin -> Cmd msg

port addScheduleItem : (UserId, ScheduleItem) -> Cmd msg


port getExtraInfo : ( UserId, Day ) -> Cmd msg
port receiveExtraInfo : (EPTypes.ExtraInfo -> msg) -> Sub msg


port uploadAvatar : (UserId) -> Cmd msg

port uploadSkinFile : (NodeId, String) -> Cmd msg
port receiveFileSkinUpload : (Common.Skin -> msg) -> Sub msg

type alias NodeId = String
