module Common.Types.Skin exposing (..)

import Date exposing (Date)

import Common.Types.User exposing (UserId)
import Common.Types.Extra as Extra exposing (Role)
import Common.Types.DateTime exposing (TimeWithoutTZ)
import Common.Types.Production exposing (ProductionSetId)

type alias Skin =
  { effectiveDt: String
  , productionSetId: String
  , skinItems: SkinItems
  }

type alias SkinItems = List SkinItem

type alias SkinItem =
  { email: Email
  , callStart: TimeWithoutTZ
  , role: Extra.Role
  , fullName: String
  , extraTalentType: ExtraTalentType
  , notes: Notes
  , rate: Rate
  }

type alias Email = String
type alias Notes = String
type alias ExtraTalentType = String
type alias Rate = String
