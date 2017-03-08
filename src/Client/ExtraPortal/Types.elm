module Client.ExtraPortal.Types exposing (..)

import Dict exposing (Dict)

import Client.Generic.WebForm.Types exposing (FieldInput)

type alias ExtraInfo =
  { extraId: String
  , timecard: TimeCard
  , profile: Profile
  , schedule: Schedule
  }


type alias TimeCard =
  { id: String
  , effectiveDt: String
  , clockinTs: Maybe String
  , clockoutTs: Maybe String
  }

type PunchAction = PunchIn | PunchOut

type alias UserID = String
type alias Schedule = {id: String, items: List ScheduleItem}
type alias ScheduleItem =
  { name: String
  , category: String
  , startTm: String
  , endTm: Maybe String
  }


type alias Avatar =
  {url: Maybe String --Should be URL
  }

type alias UserId =
    String


type alias Day =
    String

-- type FieldId =
--     FirstName | LastName | MI | Phone | Email
--   | Address1 | Address2
--
-- type alias EnumFieldId =
--   [FirstName, LastName, MI, Phone, Email
--   , Address1, Address2
--   ]

-- type alias Profile = Dict FieldId Field

type alias Profile =
  { avatar: Avatar
  , firstName: FieldInput
  , lastName: FieldInput
  }
