module Client.ExtraPortal.Types exposing (..)

type alias TimeCard =
  { clockInTs: Maybe String
  , clockoutTs: Maybe String
  }
type ClockinStatus = NotClockedIn | ClockedInNotOut | ClockedOut

type alias Schedule = List ScheduleItem
type alias ScheduleItem = {name: String, startTm: String}
