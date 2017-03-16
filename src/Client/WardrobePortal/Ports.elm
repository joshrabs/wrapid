port module Client.WardrobePortal.Ports exposing (..)

import Json.Encode as Json


port selectWardrobePhoto : (String, String) -> Cmd msg


port getAllWardrobeStatuses : () -> Cmd msg


port receiveWardrobeStatuses : (Json.Value -> msg) -> Sub msg


port receiveWardrobeStatusUpdate : (Json.Value -> msg) -> Sub msg


port checkOutWardrobe : String -> Cmd msg


port checkInWardrobe : String -> Cmd msg


port updateCheckStatus : (Json.Value -> msg) -> Sub msg
