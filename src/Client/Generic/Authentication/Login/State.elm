module Client.Generic.Authentication.Login.State exposing (..)

import Client.Generic.Authentication.Login.Types exposing (Model, Msg(..))
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            ( { model | email = Just email }, Cmd.none )

        Password password ->
            ( { model | password = Just password }, Cmd.none )

        SubmitLogin ->
            ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model


type alias Mdl =
    Material.Model
