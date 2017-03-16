module Server.API.Mutations.Avatar exposing (..)

import Ports exposing (uploadAvatar)

setUserAvatar: String -> Cmd msg
setUserAvatar userId =
  uploadAvatar(userId)
