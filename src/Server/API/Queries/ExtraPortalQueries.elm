module Server.API.Queries.ExtraPortalQueries exposing (..)

import Ports exposing (getAllExtraInfo, receiveAllExtraInfo, getExtraInfo, receiveExtraInfo)
import Client.ExtraPortal.Types exposing (UserId, ExtraInfo, Day)

-------------

fetchReqExtraInfo: (UserId, Day) -> Cmd msg
fetchReqExtraInfo =
  Ports.getExtraInfo

fetchReceiveExtraInfo: (ExtraInfo -> msg) -> Sub msg
fetchReceiveExtraInfo =
  Ports.receiveExtraInfo
-----------
