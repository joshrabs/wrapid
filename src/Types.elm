module Types exposing (..)

import Material
import Navigation as Nav
import Client.ExtraPortal.ExtraPortal as ExtraPortal
import Client.PAPortal.Types as PAPortal
import Client.Generic.Authentication.Login.Types as Login
import Time exposing (Time)
import Date exposing (Date, fromTime)


type alias Model =
    { history : List Nav.Location
    , currentDate: Maybe Date
    , currentImg : Maybe String
    , currentViewState : ViewModelState
    , title : String
    , mdl : Material.Model
    , shouldShowPortalSwitcher: Bool --Note this is only for development!
    }

type ViewModelState
    = Login Login.Model
    | ExtraPortal ExtraPortal.Model
    | PAPortal PAPortal.Model

type ViewMsg = LoginView | PAPortalView | ExtraPortalView

type Msg
    = UrlChange Nav.Location
    | Tick Time
    | SetDate Date
    | LoginMsg Login.Msg
    | ChangeView ViewMsg
    | ChildMsg ChildPortalMsg
    | ToggleNotifications
    | SelectNotification String
    | Mdl (Material.Msg Msg)
    | ShowPortalSwitcher Bool

type ChildPortalMsg = ExtraPortalMsg ExtraPortal.Msg | PAPortalMsg PAPortal.Msg




type alias Url =
    String
