module Server.API.Queries.PAPortalQueries exposing (..)

import Common.Types.Skin exposing (Skin)
import Date exposing (Date)

import Http exposing (..)
import Json.Encode as Encode exposing (encode, object, string)
import Json.Decode as Decode exposing (list, string)
import RemoteData exposing (RemoteData(..))
import Task exposing (..)



fetchDailySkin : String -> Cmd (RemoteData.WebData Skin)
fetchDailySkin d =
  let
    fakeSkin =
      {effectiveDt = "2017-25-03"
      , productionSetId="RunaBetterSet"
      ,skinItems =
        [
          {email="Bob@fakeguy.com"
          ,callStart="08:30"
          ,fullName="Bob"
          ,role="Cop"
          ,extraTalentType="BG"
          ,notes=""
          ,rate="125/12"
          }
          ,
          {email="Joe@fakedude.com"
          ,callStart="08:30"
          ,fullName="Joe"
          ,role="Cop"
          ,extraTalentType="BG"
          ,notes=""
          ,rate="125/12"
          }
        ]
      }
  in
    Task.perform (always (Success fakeSkin)) (Task.succeed ())
    -- Http.post
    --     "http://35.157.165.22/user_token"
    --     (loginBody username password)
    --     (Decode.field "jwt" Decode.string)
    --     |> RemoteData.sendRequest
