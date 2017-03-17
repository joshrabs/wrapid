module Client.WardrobePortal.Types exposing (..)

import Material


type Msg
    = SelectWardrobeStatusPhoto String
    | SelectDate String
    | ReceiveWardrobeStatuses (Result String (List WardrobeStatus))
    | WardrobeStatusUpdate (Result String WardrobeStatus)
    | CheckIn String
    | CheckOut String
    | Mdl (Material.Msg Msg)


type alias Model =
    { statuses : RemoteData (List WardrobeStatus)
    , dateFilter : DateFilter
    , mdl : Material.Model
    }


type DateFilter
    = None
    | Filter String


type RemoteData a
    = Init
    | Loading
    | Success a
    | Failure


type CheckStatus
    = NOTCHECKEDOUT
    | CHECKEDOUT
    | CHECKIN
    | Updating


type alias WardrobeStatus =
    { id : String
    , date : String
    , checkStatus : CheckStatus
    , firstName : String
    , lastName : String
    , file : Maybe String
    }
