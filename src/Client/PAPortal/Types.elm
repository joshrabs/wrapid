module Client.PAPortal.Types exposing (..)
import Client.PAPortal.Pages.SkinManager as Skin

import Date exposing (Date)

type alias Model =
  {
    currentDate: Maybe Date
  , selectedDate: SelectedDate
  , user: Profile
  , extras: Maybe (List Profile)
  , currentView: ViewState
  , skinModel : Skin.Model
  }


type alias SelectedDate = Maybe Date

type ViewState
    = LiveMonitor
    | SkinManager


type Msg
    = ChangeView ViewState
    | LoadRemoteData
    | SetSelectedDate Date
    | Profiles (List Profile)
    | SkinMsg Skin.Msg

type alias Profile =
    { id : String
    , firstName : String
    , lastName: String
    , avatarSrc : Maybe String
    }
