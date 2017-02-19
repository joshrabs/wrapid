module Client.ExtraPortal.Types exposing (..)

type alias ExtraInfo =
  {
    timecard: TimeCard
    ,profile: Profile
  }
type alias TimeCard =
  { id: String
  , effectiveDt: String
  , clockinTs: Maybe String
  , clockoutTs: Maybe String
  }

type alias UserID = String
type alias Schedule = List ScheduleItem
type alias ScheduleItem = {name: String, startTm: String}


type alias Profile =
  {
    firstName: String,
    lastName: String
  }
