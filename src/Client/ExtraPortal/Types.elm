module Client.ExtraPortal.Types exposing (..)

type alias ExtraInfo =
  {
    timecard: TimeCard
  }
type alias TimeCard =
  { clockinTs: Maybe String
  , clockoutTs: Maybe String
  }


type alias Schedule = List ScheduleItem
type alias ScheduleItem = {name: String, startTm: String}
