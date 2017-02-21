module Types exposing (..)

import Material
import Navigation as Nav
import Client.ExtraPortal.ExtraPortal as ExtraPortal
import Client.PAPortal.Types as PAPortal
import Client.Generic.Authentication.Login.Types as Login


type alias Model =
    { history : List Nav.Location
    , currentImg : Maybe String
    , currentViewState : ViewState
    , extraPortalModel : ExtraPortal.Model
    , paPortalModel : PAPortal.Model
    , title : String
    , mdl : Material.Model
    }


type Msg
    = UrlChange Nav.Location
    | LoginMsg Login.Msg
    | ChangeView ViewState
    | ExtraPortalMsg ExtraPortal.Msg
    | PAPortalMsg PAPortal.Msg
    | ToggleNotifications
    | SelectNotification String
    | Mdl (Material.Msg Msg)


type ViewState
    = LoginView
    | ExtraPortalView
    | PAPortalView


type alias Url =
    String
