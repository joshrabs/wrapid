module Client.PAPortal.Pages.Schedule.Schedule exposing (..)

type alias Time = Int

type alias Entry = { sceneName : String
                   , time : Time
                   , whoIsPlaying : String
                   }
