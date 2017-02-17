module Client.PAPortal.Types exposing (..)
import Client.PAPortal.Pages.SkinManager as Skin

type alias Model =
    { user : Profile
    , extras : List Profile
    , currentView : ViewState
    , skinModel : Skin.Model
    }


type ViewState
    = LiveMonitor
    | SkinManager


type Msg
    = ChangeView ViewState
    | GetAllProfiles
    | Profiles (List Profile)
    | SkinMsg Skin.Msg

type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }
