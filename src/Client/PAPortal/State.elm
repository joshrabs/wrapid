module Client.PAPortal.State exposing (..)

import Client.PAPortal.Types exposing (..)
import Client.PAPortal.Pages.SkinManager as Skin
import Client.PAPortal.Pages.Wrap as Wrap
import Client.PAPortal.Pages.LiveMonitor as LiveMonitor
import Ports exposing (..)

import Date exposing (Date)
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
    | ReceiveDailySkin Skin
    | SkinMsg Skin.Msg
    | WrapMsg Wrap.Msg
    | LiveMsg LiveMonitorMsg

    
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
    , currentView = LiveMonitor
    , currentSkin = Nothing
    , skinModel = Skin.initModel
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
          (model, fetchDailySkin("2017-03-07"))
        SetSelectedDate newDate ->
          initModel model.user.id (model.currentDate) (Just (Just newDate)) model.mdl
        ReceiveExtraActivity extraActivity ->
          ({model | extraActivity = Success extraActivity}, Cmd.none)

        ReceiveDailySkin skin ->
          ({model | currentSkin = Just skin}, getAllExtraInfo("2017-03-03"))

        SkinMsg subMsg ->
            let
                ( updatedSkinModel, skinCmd ) =
                    Skin.update subMsg model.skinModel
            in
                ( { model | skinModel = updatedSkinModel }
                , Cmd.none
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
                  SubmitTaskByRole item -> addScheduleItem("meow", model.liveModel.roleScheduler.scheduleItem)
                  _ -> Cmd.none
              )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [receiveAllExtraInfo ReceiveExtraActivity
  ,receiveDailySkin ReceiveDailySkin
  ]
