module Client.ExtraPortal.Types exposing (..)

type alias ExtraInfo =
  {
    timecard: TimeCard
    ,profile: Profile
    ,schedule: Schedule
  }


type alias TimeCard =
  { id: String
  , effectiveDt: String
  , clockinTs: Maybe String
  , clockoutTs: Maybe String
  }

type alias UserID = String
type alias Schedule = List ScheduleItem
type alias ScheduleItem =
  { name: String
  , category: String
  , startTm: String
  , endTm: Maybe String
  }

type alias Profile =
  {
    firstName: String,
    lastName: String
  }
