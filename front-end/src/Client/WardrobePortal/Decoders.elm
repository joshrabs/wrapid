module Client.WardrobePortal.Decoders exposing (..)

import Client.WardrobePortal.Types exposing (..)
import Json.Decode as Decode exposing (Decoder, andThen, fail, string, succeed)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optionalAt)


status : Decoder CheckStatus
status =
    let
        convert str =
            case str of
                "NOTCHECKEDOUT" ->
                    succeed NOTCHECKEDOUT

                "CHECKEDOUT" ->
                    succeed CHECKEDOUT

                "CHECKIN" ->
                    succeed CHECKIN

                _ ->
                    fail ("Invalid pattern for decoder to Wardrobe Check Status. Pattern: " ++ (toString string))
    in
        string |> andThen convert


wardrobeStatus : Decoder WardrobeStatus
wardrobeStatus =
    decode WardrobeStatus
        |> required "id" string
        |> required "date" string
        |> required "checkStatus" status
        |> requiredAt [ "user", "baseprofile", "firstName" ] string
        |> requiredAt [ "user", "baseprofile", "lastName" ] string
        |> optionalAt [ "file", "url" ] (Decode.map Just string) Nothing
