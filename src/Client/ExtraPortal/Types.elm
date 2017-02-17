module Client.ExtraPortal.Types exposing (..)

type alias ExtraInfo =
  {
    timecard: TimeCard
  }
type alias TimeCard =
  { clockInTs: Maybe String
  , clockoutTs: Maybe String
  }


type alias Schedule = List ScheduleItem
type alias ScheduleItem = {name: String, startTm: String}
