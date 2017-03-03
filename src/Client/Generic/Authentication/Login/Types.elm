module Client.Generic.Authentication.Login.Types exposing (..)

import Material


type alias Model =
    { email : String
    , password : String
    , mdl : Material.Model
    }


type Msg
    = Email String
    | Password String
    | SubmitLogin
    | Mdl (Material.Msg Msg)
