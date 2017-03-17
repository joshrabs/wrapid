module State exposing (..)

import Material
import Navigation as Nav
import Types exposing (..)
import Client.ExtraPortal.ExtraPortal as ExtraPortal
import Client.Generic.Authentication.Login.State as Login
import Client.Generic.Authentication.Login.Types as LoginTypes
import Server.API.Queries.Authentication as Server exposing (loginUser)
import Time exposing (Time, hour)
import Date exposing (Date, fromTime)
import Date.Extra.Compare as CompareDate exposing (Compare2(..))
import Task exposing (perform)
import RemoteData exposing (RemoteData(..))
import Client.PAPortal.State as PAState
import Client.Generic.Authentication.Login.State as LoginState
import Client.WardrobePortal.State as WardrobeState
import Client.WardrobePortal.Ports as WardrobePorts


defaultUserID : String
defaultUserID =
    "ciykqvsynnqo60127o3illsce"


mdlModel : Material.Model
mdlModel =
    Material.model


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    ( { history = [ location ]
      , currentImg = Nothing
      , currentDate =
            Nothing
            -- TODO: Don't forget to remove hardcode state
      , currentViewState = Login (LoginState.initModel "" "" Nothing mdlModel)
      , title = "Yo"
      , jwt = Nothing
      , mdl = mdlModel
      , shouldShowPortalSwitcher = True
      }
    , now
    )


now : Cmd Msg
now =
    Task.perform SetDate Date.now


changeView : ViewMsg -> Cmd Msg
changeView viewState =
    Task.perform (always ChangeView viewState) (Task.succeed viewState)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowPortalSwitcher bool ->
            ( { model | shouldShowPortalSwitcher = bool }, Cmd.none )

        SetDate newDate ->
            ( { model | currentDate = Just newDate }, Cmd.none )

        Tick newTime ->
            let
                newDate =
                    Date.fromTime newTime
            in
                case model.currentDate of
                    Just currentDate ->
                        if CompareDate.is Same currentDate newDate then
                            ( model, Cmd.none )
                        else
                            ( model, now )

                    Nothing ->
                        ( model, now )

        ChangeView viewState ->
            case viewState of
                LoginView ->
                    let
                        loginModel =
                            LoginState.initModel "" "" Nothing model.mdl
                    in
                        ( { model | currentViewState = Login loginModel }
                        , Cmd.none
                        )

                ExtraPortalView ->
                    let
                        ( extraPortalModel, epCmd ) =
                            ExtraPortal.initModel defaultUserID model.currentDate model.mdl
                    in
                        ( { model | currentViewState = ExtraPortal extraPortalModel }
                        , Cmd.map (\b -> ChildMsg (ExtraPortalMsg b)) epCmd
                        )

                PAPortalView ->
                    let
                        ( paModel, paCmd ) =
                            PAState.initModel defaultUserID model.currentDate Nothing model.mdl
                    in
                        ( { model | currentViewState = PAPortal paModel }
                        , Cmd.map (\b -> ChildMsg (PAPortalMsg b)) paCmd
                        )

                WardrobePortalView ->
                    ( { model | currentViewState = WardrobePortal WardrobeState.init }
                    , WardrobePorts.getAllWardrobeStatuses ()
                    )

        UrlChange location ->
            ( { model | history = location :: model.history }
            , Cmd.none
            )

        ReceiveAuthentication resp ->
            case resp of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Failure e ->
                    let
                        updateLoginView =
                            Login
                                (LoginState.initModel ""
                                    ""
                                    (Just "Login or Password is incorrect")
                                    mdlModel
                                )
                    in
                        ( { model | currentViewState = updateLoginView }
                        , Cmd.none
                        )

                Success a ->
                    ( { model | jwt = (Just a) }
                    , changeView ExtraPortalView
                    )

        --Succeed a -> (model, Cmd.)
        --  Error ->
        -- _ ->
        --     ( model, changeView ExtraPortalView )
        LoginMsg loginMsg ->
            case model.currentViewState of
                Login loginModel ->
                    let
                        ( updatedLoginModel, logMsg ) =
                            Login.update loginMsg loginModel
                    in
                        ( { model | currentViewState = Login updatedLoginModel }
                        , case loginMsg of
                            LoginTypes.SubmitLogin ->
                                (Server.loginUser loginModel.email loginModel.password)
                                    |> Cmd.map ReceiveAuthentication

                            --Server.loginUser ReceiveAuthentication loginModel.email loginModel.password
                            _ ->
                                Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        ChildMsg subMsg ->
            case subMsg of
                ExtraPortalMsg epMsg ->
                    case model.currentViewState of
                        ExtraPortal curModel ->
                            let
                                ( epModel, epCmd ) =
                                    ExtraPortal.update epMsg curModel
                            in
                                ( { model | currentViewState = ExtraPortal epModel }
                                , Cmd.map (\b -> (ChildMsg (ExtraPortalMsg b))) epCmd
                                )

                        _ ->
                            ( model, Cmd.none )

                PAPortalMsg paMsg ->
                    case model.currentViewState of
                        PAPortal curModel ->
                            let
                                ( paPortalModel, paCmd ) =
                                    PAState.update paMsg curModel
                            in
                                ( { model | currentViewState = PAPortal paPortalModel }
                                , Cmd.map (\b -> (ChildMsg (PAPortalMsg b))) paCmd
                                )

                        _ ->
                            ( model, Cmd.none )

                WardrobePortalMsg wardrobeMsg ->
                    case model.currentViewState of
                        WardrobePortal curModel ->
                            let
                                ( wardrobeModel, wardrobeCmd ) =
                                    WardrobeState.update wardrobeMsg curModel
                            in
                                ( { model | currentViewState = WardrobePortal wardrobeModel }
                                , Cmd.map (\b -> (ChildMsg (WardrobePortalMsg b))) wardrobeCmd
                                )

                        _ ->
                            model ! []

        ToggleNotifications ->
            ( model, Cmd.none )

        SelectNotification _ ->
            ( model, Cmd.none )

        Mdl message_ ->
            Material.update Mdl message_ model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every hour Tick
        , case model.currentViewState of
            ExtraPortal epModel ->
                Sub.map (\b -> ChildMsg (ExtraPortalMsg b)) (ExtraPortal.subscriptions epModel)

            PAPortal paModel ->
                Sub.map (\b -> ChildMsg (PAPortalMsg b)) (PAState.subscriptions paModel)

            Login loginModel ->
                Sub.none

            WardrobePortal wardrobeModel ->
                Sub.map (\b -> ChildMsg (WardrobePortalMsg b)) (WardrobeState.subscriptions wardrobeModel)
        , Material.subscriptions Mdl model
        ]
