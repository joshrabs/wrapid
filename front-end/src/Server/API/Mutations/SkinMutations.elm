module Server.API.Mutations.SkinMutations exposing (..)

import Ports exposing (uploadSkin, uploadSkinFile, receiveDailySkin)
import Common.Types.Skin exposing (Skin)

-------------

uploadSkin: Skin -> Cmd msg
uploadSkin skin =
  Ports.uploadSkin(skin)

receiveUploadedSkin: (Maybe Skin -> msg) -> Sub msg
receiveUploadedSkin =
  Ports.receiveDailySkin
-----------
