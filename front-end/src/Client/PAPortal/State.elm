module Client.PAPortal.State exposing (..)

import Client.PAPortal.Types as PATypes exposing (..)
import Client.PAPortal.Pages.SkinManager as Skin
import Client.PAPortal.Pages.Wrap as Wrap
import Client.PAPortal.Pages.LiveMonitor as LiveMonitor
import Client.PAPortal.Pages.SkinUploadPage as SkinUploadPage
import Client.Utilities.DateTime exposing (frmtDate)
import Common.Types.Skin as Common exposing (Skin)
import Ports exposing (receiveFileSkinUpload, uploadSkinCSV, getAllExtraInfo, addScheduleItem)
import Server.API.Queries.PAPortalQueries as ServerQueries exposing (fetchDailySkin)
import Server.API.Mutations.SkinMutations as ServerMutations exposing (uploadSkin, receiveUploadedSkin)
import RemoteData exposing (WebData, toMaybe)

import Date exposing (Date)
import Task exposing (perform, succeed)
import Material

type alias Model =
  {
    currentDate: Maybe Date
  , selectedDate: SelectedDate
  , user: PAProfile
  , extraInfo: RemoteData (List ExtraInfo)
  -- , extraActivity: RemoteData (List ExtraActivity)
  , currentView: ViewState
  , currentSkin: Maybe Common.Skin
  , skinModel : Skin.Model
  , wrapModel : Wrap.Model
  , liveModel : LiveMonitorState
  , mdl : Material.Model
  }


type Msg
    = ChangeView ViewState
    | LoadRemoteData
    | SetSelectedDate Date
    | ReceiveExtraInfo (List ExtraInfo)
    | ReceiveDailySkin (RemoteData.WebData Common.Skin)
    | SkinMsg Skin.Msg
    | WrapMsg Wrap.Msg
    | LiveMsg LiveMonitorMsg
    | SkinUploadPageMsg SkinUploadPage.Msg
    | ReceiveFileSkinUpload Common.Skin


initModel: String -> Maybe Date -> Maybe SelectedDate -> Material.Model -> (Model, Cmd Msg)
initModel userId currentDate selectedDate materialModel =
  let
    user =
      {id=userId
      , firstName="Jeff"
      , lastName="PaGuy"
      , avatarSrc=Nothing
      }
  in
    (
    { user = user
    , extraInfo = Loading
    , currentDate = currentDate
    , selectedDate =
        case selectedDate of
          Just date -> date
          Nothing -> currentDate
    , currentView = Initializing
    , currentSkin = Nothing
    , skinModel = Skin.initModel Nothing (frmtDate currentDate)
    , wrapModel = Wrap.initModel
    , liveModel = LiveMonitor.initState materialModel
    , mdl = materialModel
    }
    , Task.perform (always LoadRemoteData) (Task.succeed ())
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeView view ->
            ( { model | currentView = view }, Cmd.none )
        LoadRemoteData ->
            let
              date = frmtDate model.currentDate
              l = Debug.log "DATE!!!!!!: " date
            in
              (model, Cmd.map (\s -> ReceiveDailySkin s) (fetchDailySkin date))
        SetSelectedDate newDate ->
          initModel model.user.id (model.currentDate) (Just (Just newDate)) model.mdl
        ReceiveExtraInfo extraInfo ->
            ({model | extraInfo = Success extraInfo}, Cmd.none)

        ReceiveDailySkin webSkin ->
          let
              skin = webSkin |> toMaybe

              date = frmtDate model.currentDate
              dLog = Debug.log "d: " date
          in

          ({model |
            currentSkin = skin
            , currentView = SkinManager
            , skinModel=Skin.initModel skin date}
            , getAllExtraInfo(date)
          )

        SkinMsg subMsg ->
            let
                ( updatedSkinModel, skinCmd ) =
                    Skin.update subMsg model.skinModel

                dateStr = frmtDate model.currentDate
            in
                ( { model | skinModel = updatedSkinModel }
                , case subMsg of
                    Skin.UploadSkin ->
                      let
                          roleLog = Debug.log "SKIN!!: " (Skin.rolesToSkin updatedSkinModel.roles dateStr)
                      in
                          ServerMutations.uploadSkin (Skin.rolesToSkin updatedSkinModel.roles dateStr)
                    _ -> Cmd.none
                )
        WrapMsg subMsg ->
          let
              updatedWrapModel =
                  Wrap.update subMsg model.wrapModel
          in
              ( { model | wrapModel = updatedWrapModel }
              , Cmd.none
              )
        LiveMsg subMsg ->
          let
              (updatedLMModel, lmCmd) =
                  LiveMonitor.update subMsg model.liveModel

              log3 = Debug.log "Schedule Item" model.liveModel.roleScheduler.scheduleItem
          in
              ( { model | liveModel = updatedLMModel }
              , case subMsg of
                  SubmitTaskByRole item ->
                    addScheduleItem("meow", model.liveModel.roleScheduler.scheduleItem)
                  _ -> Cmd.none
              )
        SkinUploadPageMsg subMsg ->
          case Debug.log "msg" subMsg of
            SkinUploadPage.CreateSkin ->
              let
                  l = Debug.log "cREATE SKIN!!!" subMsg
              in
                ({model | currentView=SkinManager}, Cmd.none)
            SkinUploadPage.UploadSkin ->
                (model, uploadSkinCSV("NodeSkinCSVUpload", frmtDate model.currentDate))
        ReceiveFileSkinUpload skin ->
          let
            l = Debug.log "Skin!" skin
          in
            (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [receiveFileSkinUpload ReceiveFileSkinUpload
  ]
