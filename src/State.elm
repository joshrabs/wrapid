module State exposing (..)

import Material
import Navigation as Nav
import Types exposing (..)
import Client.ExtraPortal.ExtraPortal as ExtraPortal

import Time exposing (Time, hour)
import Date exposing (Date, fromTime)
import Date.Extra.Compare as CompareDate exposing (Compare2(..))
import Task exposing (perform)


-- import Client.PAPortal.Types as PATypes

import Client.PAPortal.State as PAState


defaultUserID : String
defaultUserID =
    "ciykqvsynnqo60127o3illsce"


init : Nav.Location -> ( Model, Cmd Msg )
init location =
    ( { history = [ location ]
      , currentImg = Nothing
      , currentDate = Nothing
      , currentViewState = LoginView
      , extraPortalModel = (ExtraPortal.initModel defaultUserID)
      , paPortalModel = (PAState.initModel "word")
      , title = "Yo"
      , mdl = Material.model
      }
    , now
    )

now : Cmd Msg
now =
  Task.perform SetDate Date.now


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

      Mdl message_ ->
          Material.update Mdl message_ model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every hour Tick
        , Sub.map PAPortalMsg (PAState.subscriptions model.paPortalModel)
        , Sub.map ExtraPortalMsg (ExtraPortal.subscriptions model.extraPortalModel)
        , Material.subscriptions Mdl model
        ]
