module Client.PAPortal.State exposing (..)

import Client.PAPortal.Types exposing (..)
import Client.PAPortal.Pages.SkinManager as Skin
import Client.PAPortal.Pages.Wrap as Wrap
import Client.PAPortal.Pages.LiveMonitor as LiveMonitor
import Client.PAPortal.Pages.SkinUploadPage as SkinUploadPage
import Ports exposing (..)
import Server.API.Mutations.SkinMutations as Server exposing (uploadSkin, receiveUploadedSkin)

import Date exposing (Date)
import Date.Extra.Format exposing (format)
import Date.Extra.Config.Config_en_us exposing (config)
import Task exposing (perform, succeed)
import Material

type alias Model =
  {
    currentDate: Maybe Date
  , selectedDate: SelectedDate
  , user: PAProfile
  , extraActivity: RemoteData (List ExtraActivity)
  , currentView: ViewState
  , currentSkin: Maybe Skin
  , skinModel : Skin.Model
  , wrapModel : Wrap.Model
  , liveModel : LiveMonitorState
  , mdl : Material.Model
  }


type Msg
    = ChangeView ViewState
    | LoadRemoteData
    | SetSelectedDate Date
    | ReceiveExtraActivity (List ExtraActivity)
    | ReceiveDailySkin (Maybe Skin)
    | SkinMsg Skin.Msg
    | WrapMsg Wrap.Msg
    | LiveMsg LiveMonitorMsg
    | SkinUploadPageMsg SkinUploadPage.Msg


initModel: String -> Maybe Date -> Maybe SelectedDate -> Material.Model -> (Model, Cmd Msg)
initModel userId currentDate selectedDate materialModel =
  let
    user =
      {id=userId
      , firstName="Jeff"
      , lastName="PaGuy"
      , avatarSrc=Just "https://files.graph.cool/ciykpioqm1wl00120k2e8s4la/ciyvfw6ab423z01890up60nza"
      }
  in
    (
    { user = user
    , extraActivity = Loading
    , currentDate = currentDate
    , selectedDate =
        case selectedDate of
          Just date -> date
          Nothing -> currentDate
    , currentView = Initializing
    , currentSkin = Nothing
    , skinModel = Skin.initModel Nothing (getDateStr currentDate)
    , wrapModel = Wrap.initModel
    , liveModel = LiveMonitor.initState materialModel
    , mdl = materialModel
    }
    , Task.perform (always LoadRemoteData) (Task.succeed ())
    )

getDateStr: Maybe Date -> String
getDateStr date =
    date
      |> Maybe.map (Date.Extra.Format.format Date.Extra.Config.Config_en_us.config "%Y-%m-%d")
      |> Maybe.withDefault ""

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeView view ->
            ( { model | currentView = view }, Cmd.none )
        LoadRemoteData ->
            let
              date = getDateStr model.currentDate
              l = Debug.log "DATE!!!!!!: " date
            in
              (model, fetchDailySkin(date))
        SetSelectedDate newDate ->
          initModel model.user.id (model.currentDate) (Just (Just newDate)) model.mdl
        ReceiveExtraActivity extraActivity ->
          ({model | extraActivity = Success extraActivity}, Cmd.none)

        ReceiveDailySkin skin ->
          let
              newView =
                case skin of
                  Just skin -> SkinManager
                  Nothing -> SkinUploadPage

              date = getDateStr model.currentDate
              dLog = Debug.log "d: " date
          in

          ({model | currentSkin = skin, currentView = newView, skinModel=Skin.initModel skin date}, getAllExtraInfo(date))

        SkinMsg subMsg ->
            let
                ( updatedSkinModel, skinCmd ) =
                    Skin.update subMsg model.skinModel

                dateStr = getDateStr model.currentDate
            in
                ( { model | skinModel = updatedSkinModel }
                , case subMsg of
                    Skin.UploadSkin ->
                      let
                          roleLog = Debug.log "SKIN!!: " (Skin.rolesToSkin updatedSkinModel.roles dateStr)
                          d = Debug.log "datestr!!: " dateStr
                      in
                          Server.uploadSkin (Skin.rolesToSkin updatedSkinModel.roles dateStr)
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
              ({model | currentView=SkinManager}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [receiveAllExtraInfo ReceiveExtraActivity
  ,receiveDailySkin ReceiveDailySkin
  ]
