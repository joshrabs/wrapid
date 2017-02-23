module Client.PAPortal.Types exposing (..)
import Client.PAPortal.Pages.SkinManager as Skin
import Client.PAPortal.Pages.Wrap as Wrap

import Date exposing (Date)
import Material

type RemoteData a = Loading | Success a

type alias Model =
  {
    currentDate: Maybe Date
  , selectedDate: SelectedDate
  , user: Profile
  , extras: RemoteData ExtraInfo
  , currentView: ViewState
  , skinModel : Skin.Model
  , wrapModel : Wrap.Model
  , mdl : Material.Model
  }

type alias ExtraInfo = Maybe (List Profile)
type alias SelectedDate = Maybe Date

type ViewState
    = LiveMonitor
    | SkinManager
    | Wrap


type Msg
    = ChangeView ViewState
    | LoadRemoteData
    | SetSelectedDate Date
    | Profiles (List Profile)
    | SkinMsg Skin.Msg
    | WrapMsg Wrap.Msg

type alias Profile =
    { id : String
    , firstName : String
    , lastName: String
    , avatarSrc : Maybe String
    }
