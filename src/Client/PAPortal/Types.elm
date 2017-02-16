module Client.PAPortal.Types exposing (..)


type alias Model =
    { user : Profile
    , extras : Maybe (List Profile)
    , currentView : ViewState
    }


type ViewState
    = LiveMonitor
    | SkinManager


type Msg
    = ChangeView ViewState
    | GetAllProfiles
    | Profiles (List Profile)


type alias Profile =
    { id : String
    , firstName : String
    , url : Maybe String
    }
