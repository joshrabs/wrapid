module Client.PAPortal.State exposing (..)

import Client.PAPortal.Types exposing (..)
import Ports exposing (..)


initModel : String -> Model
initModel userId =
    let
        user =
            { id = userId
            , firstName = "Jeff"
            , url = Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"
            }
    in
        { user = user
        , extras = Nothing
        , currentView = LiveMonitor
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeView view ->
            ( { model | currentView = view }, Cmd.none )

        GetAllProfiles ->
            let
                _ =
                    Debug.log "Running get all profiles!"
            in
                ( { model | user = { id = model.user.id, firstName = "Bob", url = model.user.url } }
                , getAllProfiles ()
                )

        Profiles list ->
            ( { model | extras = Just list }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveNames Profiles
