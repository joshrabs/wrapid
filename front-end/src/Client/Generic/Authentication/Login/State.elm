module Client.Generic.Authentication.Login.State exposing (..)

import Client.Generic.Authentication.Login.Types exposing (Model, Msg(..), ViewState(..))
import Material


initModel : String -> String -> Maybe String -> Material.Model -> Model
initModel email password error mdlModel =
    { email = email
    , password = password
    , mdl = mdlModel
    , error = error
    , viewState = InUse
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            ( { model | email = email }, Cmd.none )

        Password password ->
            ( { model | password = password }, Cmd.none )

        SubmitLogin ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model
