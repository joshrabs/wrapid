module Client.Generic.Authentication.Login.Types exposing (..)

import Material


type alias Model =
    { email : Maybe String
    , password : Maybe String
    , mdl : Material.Model
    }


type Msg
    = Email String
    | Password String
    | SubmitLogin
    | Mdl (Material.Msg Msg)
