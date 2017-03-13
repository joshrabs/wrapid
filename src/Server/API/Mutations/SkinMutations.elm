module Server.API.Mutations.SkinMutations exposing (..)

import Ports exposing (uploadSkin, receiveUploadedSkin)
import Client.PAPortal.Types exposing (Skin)

-------------

uploadSkin: Skin -> Cmd msg
uploadSkin =
  Ports.uploadSkin

receiveUploadedSkin: (Maybe Skin -> msg) -> Sub msg
receiveUploadedSkin =
  Ports.receiveUploadedSkin
-----------
