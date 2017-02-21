module State exposing (..)

import Navigation as Nav
import Types exposing (..)
import Client.ExtraPortal.ExtraPortal as ExtraPortal


-- import Client.PAPortal.Types as PATypes

import Client.PAPortal.State as PAState


defaultUserID : String
defaultUserID =
    "ciykqvsynnqo60127o3illsce"


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    ( Model [ location ] Nothing LoginView (ExtraPortal.initModel defaultUserID) (PAState.initModel "word") "Yo"
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeView viewState ->
            case viewState of
                LoginView ->
                    ( { model | currentViewState = viewState }, Cmd.none )

                ExtraPortalView ->
                    let
                        ( extraPortalModel, cmd ) =
                            ExtraPortal.update ExtraPortal.LoadRemoteData model.extraPortalModel
                    in
                        ( { model | extraPortalModel = extraPortalModel, currentViewState = viewState }, cmd )

                PAPortalView ->
                    ( { model | currentViewState = viewState }, Cmd.none )

        UrlChange location ->
            ( { model | history = location :: model.history }
            , Cmd.none
            )

        LoginMsg loginMsg ->
            ( model, Cmd.none )

        ExtraPortalMsg epMsg ->
            let
                ( extraPortalModel, epCmd ) =
                    ExtraPortal.update epMsg model.extraPortalModel
            in
                ( { model | extraPortalModel = extraPortalModel }, Cmd.map (\b -> ExtraPortalMsg b) epCmd )

        PAPortalMsg paMsg ->
            let
                ( paPortalModel, paCmd ) =
                    PAState.update paMsg model.paPortalModel
            in
                ( { model | paPortalModel = paPortalModel }, Cmd.map (\b -> PAPortalMsg b) paCmd )

        ToggleNotifications ->
            ( model, Cmd.none )

        SelectNotification _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map (\pas -> PAPortalMsg pas) (PAState.subscriptions model.paPortalModel)
        , Sub.map (\eps -> ExtraPortalMsg eps) (ExtraPortal.subscriptions model.extraPortalModel)
        ]
