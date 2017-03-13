module Client.PAPortal.Types exposing (..)
import Client.PAPortal.Pages.Wrap as Wrap
import Client.ExtraPortal.Types exposing (ScheduleItem, TimeCard, Profile, Schedule)

import Date exposing (Date)
import Material

type RemoteData a = Loading | Success a


type alias ExtraActivity =
  { extraId: String
  , timecard: TimeCard
  , schedule: Schedule
  }


type alias SelectedDate = Maybe Date

type ViewState
    = Initializing
    | LiveMonitor
    | SkinManager
    | Wrap

type alias PAProfile =
    { id : String
    , firstName : String
    , lastName: String
    , avatarSrc : Maybe String
    }

type alias ExtraProfile =
  {extraId: String
  ,firstName: String
  ,lastName: String
  }

type alias Skin =
  {effectiveDt: String
  ,skinItems: List SkinItem
  }

type alias SkinItem =
  {userId: String
  ,firstName: String
  ,lastName: String
  ,part: String
  ,pay: String
  ,avatar: {url: Maybe String}
  }


--Live Monitor
type alias LiveMonitorState =
    { isAddingTask: Bool
    , mdl : Material.Model
    , tableFilter: LiveTableFilter
    , roleScheduler: RoleScheduler
    }

type alias LiveTableFilter = String

type alias RoleScheduler =
  {role: String
  ,scheduleItem: ScheduleItem
  }

type alias LiveExtraTable =
    List ExtraInfoItem


type alias ExtraInfoItem =
    { firstName : String
    , lastName: String
    , part: String
    , imgSrc : Maybe String
    , isClockedIn : Bool
    }

type alias ExtraInfo =
  {extraId: String
  --,profile: Profile
  ,firstName: String
  ,lastName: String
  ,avatar: {url: Maybe String}
  ,role: String
  ,pay: String
  }

type alias ExtrasSnapStatModel =
    { totalExtras : Int
    , clockedIn : Int
    , holdClothes : Int
    , missingForms : Int
    }

type ScheduleTimeParam = StartTm | EndTm

type LiveMonitorMsg =
    Mdl (Material.Msg LiveMonitorMsg)
  | ToggleAddingTask
  | SubmitTaskByRole ScheduleItem
  | SetTableFilter LiveTableFilter
  | SetSchedulerRole String
  | SetSchedulerTime ScheduleTimeParam String
  | SetSchedulerCategory String
  | SetSchedulerName String
