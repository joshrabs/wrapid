module Types exposing (..)

import Navigation as Nav
import Client.ExtraPortal.ExtraPortal as ExtraPortal
import Client.PAPortal.PAPortal as PAPortal


type alias Model =
    { history : List Nav.Location
    , currentImg : Maybe String
    , currentViewState : ViewState
    , extraPortalModel : ExtraPortal.Model
    , paPortalModel : PAPortal.Model
    , title : String
    }


type ViewState
    = LoginView
    | ExtraPortalView
    | PAPortalView


type alias Url =
    String


type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }
