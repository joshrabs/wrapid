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


-- import Client.PAPortal.Types as PATypes

import Client.PAPortal.State as PAState
import Client.Generic.Authentication.Login.State as LoginState


defaultUserID : String
defaultUserID =
    "ciykqvsynnqo60127o3illsce"

mdlModel = Material.model

init : Nav.Location -> ( Model, Cmd Msg )
init location =
    ( { history = [ location ]
      , currentImg = Nothing
      , currentDate = Nothing
      , currentViewState = Login (LoginState.initModel "" "" mdlModel)
      , title = "Yo"
      , mdl = mdlModel
      , shouldShowPortalSwitcher = True
      }
    , now
    )

now : Cmd Msg
now =
  Task.perform SetDate Date.now

changeView: ViewMsg -> Cmd Msg
changeView viewState = Task.perform (always ChangeView viewState) (Task.succeed viewState)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      ShowPortalSwitcher bool -> ({model | shouldShowPortalSwitcher = bool}, Cmd.none)
      SetDate newDate ->
        ({model | currentDate = Just newDate}, Cmd.none)
      Tick newTime ->
        let
          newDate = Date.fromTime newTime
        in
          case model.currentDate of
            Just currentDate ->
              if CompareDate.is Same currentDate newDate
              then
                (model, Cmd.none)
              else
                (model, now)
            Nothing ->
              (model, now)
      ChangeView viewState ->
          case viewState of
              LoginView ->
                  let
                      loginModel =
                          LoginState.initModel "" "" model.mdl
                  in
                      ( { model | currentViewState = Login loginModel}
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
                  (paModel, paCmd) =
                    PAState.initModel defaultUserID model.currentDate Nothing model.mdl
                in
                  ( { model | currentViewState = PAPortal paModel }
                  , Cmd.map (\b -> ChildMsg (PAPortalMsg b)) paCmd
                  )


      UrlChange location ->
          ( { model | history = location :: model.history }
          , Cmd.none
          )

      ReceiveAuthentication resp ->
        case resp of
          --Succeed a -> (model, Cmd.)
        --  Error ->
          _ -> (model, changeView ExtraPortalView)

      LoginMsg loginMsg ->
        case model.currentViewState of
          Login loginModel ->
            let
              (updatedLoginModel, logMsg) = Login.update loginMsg loginModel
            in
              ( {model | currentViewState = Login updatedLoginModel}
              , case loginMsg of
                  LoginTypes.SubmitLogin ->
                    (Server.loginUser loginModel.email loginModel.password)
                      |> Cmd.map ReceiveAuthentication
                    --Server.loginUser ReceiveAuthentication loginModel.email loginModel.password
                  _ -> Cmd.none
              )

          _ -> (model, Cmd.none)

      ChildMsg subMsg->
        case subMsg of
          ExtraPortalMsg epMsg ->
            case model.currentViewState of
              ExtraPortal curModel ->
                let
                    ( epModel, epCmd ) = ExtraPortal.update epMsg curModel
                in
                    ( { model | currentViewState = ExtraPortal epModel }
                      , Cmd.map (\b -> (ChildMsg (ExtraPortalMsg b))) epCmd )

              _ -> (model, Cmd.none)

          PAPortalMsg paMsg ->
              case model.currentViewState of
                PAPortal curModel ->
                  let
                      ( paPortalModel, paCmd ) = PAState.update paMsg curModel
                  in
                      ( { model | currentViewState = PAPortal paPortalModel }
                        , Cmd.map (\b -> (ChildMsg (PAPortalMsg b))) paCmd )

                _ -> (model, Cmd.none)


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
        , Material.subscriptions Mdl model
        ]
