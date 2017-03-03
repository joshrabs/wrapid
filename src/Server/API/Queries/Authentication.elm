module Server.API.Queries.Authentication exposing (..)

import Http exposing (..)
import Json.Encode as Encode exposing (encode, object, string)
import Json.Decode as Decode exposing (list, string)
import RemoteData exposing (sendRequest)

type alias Username = String
type alias Password = String

-- loginPostReq : Body -> Request String
-- loginPostReq body =
--   request
--     { method = "POST"
--     , headers =
--         [
--           header "Content-Type" "application/json"
--           ,header "charset" "utf-8"
--         ]
--     , url = "http://35.157.165.22//user_token"
--     , body = body
--     , expect = expectJson (Decode.at ["auth", "email"] Decode.string)
--     , timeout = Nothing
--     , withCredentials = False
--     }

loginBody: Username -> Password -> Http.Body
loginBody username password =
  jsonBody
    (object
      [("auth", Encode.object
        [ ("email", Encode.string username)
        , ("password", Encode.string password)
        ]
        )
      ]
    )

-- loginUser: (Result Error String -> msg) -> Username -> Password -> Cmd msg
-- loginUser cmsg username password =
--   Http.send cmsg (loginPostReq (loginBody username password))

loginUser: Username -> Password ->  Cmd (RemoteData.WebData String)
loginUser username password =
    Http.post
      "https://35.157.165.22//user_token" (loginBody username password) Decode.string
        |> RemoteData.sendRequest
